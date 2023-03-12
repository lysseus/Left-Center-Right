#lang racket

;;;
;;; LEFT-CENTER-RIGHT
;;;
;;; Game begins with 9 players in auto mode.
;;; * Pressing <esc> will toggle auto mode.
;;; * Pressing <space> in manual mode will step manually.
;;; * Pressing <n> where n {3,9} will start a new game
;;;   with n players in manual mode.
;;;

(require 2htdp/image
         2htdp/universe
         utils/math/parametrics
         utils/2htdp/image
         utils/vector)

(struct world (round# auto? roll? player# winner# roll tokens wins center r1 r2 θ0)
  #:mutable #:transparent)

(define/contract (new-world (num-of-players 3) (auto? #f) (r1 24))
  (->* () ((between/c 3 12) boolean? (and/c natural? (between/c 10 40))) any)
  (world 1 auto? #t 0 #f '() (make-vector num-of-players 3) (make-vector num-of-players 0)  0 r1 (* 5 r1) -90))

(define (play ws)  
  (cond
    [(and (world-roll? ws) (world-winner# ws))
     (winner ws)]
    [(and (world-roll? ws))
     (roll-dice ws)
     (set-world-roll?! ws (not (world-roll? ws)))]
    [else
     (check-for-winner ws)
     (set-world-roll! ws '())
     ;; Set next player.
     (define vlen (vector-length (world-tokens ws)))
     (for ([p# (range vlen)])
       (set-world-player#! ws (modulo (add1 (world-player# ws)) vlen))
       #:break (> (vector-ref (world-tokens ws) (world-player# ws)) 0)
       (void))
     (set-world-roll?! ws (not (world-roll? ws)))])
  ws)

(define (tick-handler ws)
  (if (world-auto? ws)
      (play ws)
      ws))

(define (key-handler ws ke)  
  (cond    
    [(and (string->number ke) (< 2 (string->number ke) 10))
     (new-world (string->number ke))]
    [(key=? ke "escape") (set-world-auto?! ws (not (world-auto? ws))) ws]
    [else (play ws)]))





(define (check-for-winner ws)
  (cond
    [(= 1 (count (compose not zero?) (vector->list (world-tokens ws))))
     (define-values (winner# v) (vector-findf (world-tokens ws) (λ (v) (> v 0))))
     (when (integer? winner#)
       (set-world-winner#! ws winner#)       
       (vector-set! (world-wins ws) (world-winner# ws)
                    (add1 (vector-ref (world-wins ws) winner#))))]
    [else (set-world-winner#! ws #f)]))

(define (winner ws)
  (define winner# (world-winner# ws))
  (define tokens (world-tokens ws))
  (vector-fill! tokens 3)  
  (set-world-center! ws 0)
  (set-world-round#! ws (add1 (world-round# ws)))
  (set-world-winner#! ws #f))

(define (place-image-on-circle r θ img1 img2 (pinhole? #f))
  (define x0 (quotient (image-width img2) 2))
  (define y0 (quotient (image-height img2) 2))
  (define Ρ-unit-circle-r (Ρ-unit-circle r))
  (define x (round (second (first (Ρ-unit-circle-r θ)))))
  (define y (round (second (second (Ρ-unit-circle-r θ)))))
  (place-image img1
               (+ x0 x)
               (+ y0 y)
               (if (false? pinhole?)
                   img2
                   (put-pinhole x0 y0 img2))))

(define (draw-equidistant-on-circle #:pinhole? (pinhole? #f)                                    r θ0 . imgs)
  (define bg (last imgs))
  (define fg (drop-right imgs 1))
  (define m (quotient 360 (length fg)))
  (define-values (ds v)
    (for/fold ([acc (list θ0)] [v θ0])
              ([n (range (sub1 (length fg)))])
      (define k (modulo (+ v m) 360))
      (values (append acc (list k))
              k)))  
  (define img (for/fold ([bg bg])
                        ([f fg]
                         [d ds])                
                (place-image-on-circle r d f bg)))  
  (if (false? pinhole?)
      img
      (put-pinhole (quotient (image-width img) 2)
                   (quotient (image-height img) 2)
                   img)))

(define FONT-SIZE 24)
(define FONT-COLOR 'white)
(define PLAYER-FONT-COLOR 'black)
(define BG-COLOR 'black)
(define PLAYER-BG-COLOR 'yellow)
(define OUTLINE-COLOR 'white)
(define PLAYER-OUTLINE-COLOR 'darkorange)

(define MT-WIDTH  800)
(define MT-HEIGHT 600)
(define MT (empty-scene MT-WIDTH MT-HEIGHT 'black))

(define RULES
  (overlay
   (overlay/fit #:w-pad 20 #:h-pad 20
   (text "Game begins with 9 players in auto mode.~%
#\u2022 Pressing <esc> will toggle auto mode.~%
#\u2022 Pressing <space> in manual mode will step manually.~%
#\u2022 Pressing <n> where n {3,9} will start a new game~%
    with n players in manual mode."
                    16 FONT-COLOR)
   (overlay (rounded-rectangle 392 92 0.5 'solid 'black)
            (rounded-rectangle 400 100 0.5 'solid FONT-COLOR)))
   (rectangle 420 120 'solid 'transparent)))

(define (draw-small-circle txt
                           radius
                           font-color
                           outline-color
                           bg-color)
  (overlay/fit #:w-pad 8
               #:h-pad 8
               (text (~a txt) radius font-color)
               (overlay (circle radius 'outline outline-color)
                        (circle (sub1 radius) 'outline outline-color)
                        (circle radius 'solid bg-color))))

(define (draw-player-circle ws v n)
  (draw-small-circle v
                     (world-r1 ws)
                     (if (= (world-player# ws) n)
                         PLAYER-FONT-COLOR
                         FONT-COLOR)
                     (if (= (world-player# ws) n)
                         PLAYER-OUTLINE-COLOR
                         OUTLINE-COLOR)
                     (if (= (world-player# ws) n)
                         PLAYER-BG-COLOR
                         BG-COLOR)))

(define (draw-players ws)
  (define imgs (for/list ([v (world-tokens ws)]
                          [n (in-naturals)])
                 (draw-player-circle ws v n)))
  (define bg (overlay
              (text (~a (world-center ws)) (world-r1 ws) FONT-COLOR)
              (circle (world-r1 ws) 'outline FONT-COLOR)
              (circle (world-r1 ws) 'solid BG-COLOR)
              (circle (world-r2 ws) 'outline FONT-COLOR)
              (square (+ (* 2 (world-r1 ws)) (* 2 (world-r2 ws))) 'solid BG-COLOR)))
  (apply draw-equidistant-on-circle
         (world-r2 ws)
         (world-θ0 ws)         
         (append imgs (list bg))))

(define (draw-dice #:font-size (font-size 40)
                   #:font-color (font-color 'white)
                   ns)
  (for/list ([n ns])
    (text (~a (case n
                [(1) #\u2680]
                [(2) #\u2681]
                [(3) #\u2682]
                [(4) #\u2683]
                [(5) #\u2684]
                [(6) #\u2685]))
          font-size font-color)))

(define (roll-n-dice n)
  (for/list ([n (range n)])
    (random 1 6)))

(define (roll-dice ws)  
  (define tokens (world-tokens ws))
  (define num-of-players (vector-length tokens))
  (define player# (world-player# ws))
  (define left# (modulo (sub1 player#) num-of-players))
  (define right# (modulo (add1 player#) num-of-players))
  (define num-of-tokens (vector-ref tokens player#))
  (define n (cond
              [(< num-of-tokens 3) num-of-tokens]
              [else 3]))
  (define roll (roll-n-dice n))  
  (define-values (left-amt center-amt right-amt no-amt)
    (for/fold ([l 0] [c 0] [r 0] [n 0])
              ([v roll])
      (define ans (case v
                    [(1 2 3) '(0 0 0 1)]
                    [(4)    '(1 0 0 0)]
                    [(5)    '(0 1 0 0)]
                    [(6)   '(0 0 1 0)]))      
      (values (+ (first ans) l)
              (+ (second ans) c)
              (+ (third ans) r)
              (+ (fourth ans) n))))  
  (set-world-center! ws (+ center-amt (world-center ws)))
  (vector-set! tokens left# (+ left-amt (vector-ref tokens left#)))
  (vector-set! tokens right# (+ right-amt (vector-ref tokens right#)))  
  (vector-set! tokens player# (- (vector-ref tokens player#) left-amt center-amt right-amt))
  (set-world-roll! ws (sort roll <))
  ws)

(define (draw-dice-roll ws)  
  (define dice-images (draw-dice (world-roll ws)))  
  (define imgs
    (for/list ([r (world-roll ws)]
               [img dice-images])
      (define txt (case r
                    [(1 2 3) "No action"]
                    [(4) "Left"]
                    [(5) "Center"]
                    [(6) "Right"]))
      (beside img (text (format "  ~a" txt) (world-r1 ws) FONT-COLOR))))
  (cond
    [(empty? (world-roll ws)) empty-image]
    [(= (length (world-roll ws)) 1)
     (car imgs)]
    [else (apply above/align "left" imgs)]))

(define (draw-wins ws)
  (above
   (text "Player\tWins" (world-r1 ws) FONT-COLOR)
   (apply above (for/list ([v (world-wins ws)]
                           [n (in-naturals 1)])
                  (define txt (format "  ~a\t\t~a" n v))
                  (text txt (world-r1 ws) FONT-COLOR)))))

(define (draw-winner ws)
  (define winner# (add1 (world-winner# ws)))
  (define size (* 16 (world-r1 ws)))
  (define center-amt (world-center ws))
  (define img (above/align "left"
                           (text (format "Player ~a has won the ~a tokens." winner# center-amt)
                                 (world-r1 ws)
                                 FONT-COLOR)
                           (text "All players receive 3 tokens"
                                 (world-r1 ws)
                                 FONT-COLOR)
                           (text "for the next round of play."
                                 (world-r1 ws)
                                 FONT-COLOR)))
  (above/align "left"
               (draw-wins ws)
               (overlay/fit img
                            (square size 'solid 'transparent))
               (square (world-r1 ws) 'solid 'transparent)))

(define (draw-players-panel ws)
  (above (draw-players ws)
         (square (* 2 (world-r1 ws)) 'solid 'transparent)
         (text (format "ROUND ~a" (world-round# ws)) (world-r1 ws) FONT-COLOR)))

(define (draw-world ws)
  (beside/align "top"
                (draw-players-panel ws)
                (square (* 2 (world-r1 ws)) 'solid 'transparent)
                (if (false? (world-winner# ws))
                    (draw-dice-roll ws)
                    (draw-winner ws))))

(define (render ws)
  (place-image/align (draw-world ws)
                     20 20 "left" "top"
                     (overlay/align "center" "bottom" RULES MT)))

(big-bang (new-world 9 #t)
  (to-draw render)
  (on-tick tick-handler 2)
  (on-key key-handler)
  (name "LEFT CENTER RIGHT"))
