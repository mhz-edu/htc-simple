;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-romanmhz) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define TRANSP-BG (rectangle WIDTH HEIGHT 0 "white"))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; Functions:

;; Game -> Game
;; start the world with (main G0)
;; 
(define (main ws)
  (big-bang ws                   ; Game
    (on-tick   tock)     ; Game -> Game
    (to-draw   render)   ; Game -> Image
    (stop-when invader-bottom?)      ; Game -> Boolean
    (on-key    handle-key)))    ; Game KeyEvent -> Game

;; Game -> Game
;; produce the next game state by advancing missiles, invaders, tank

;; template from game definition
#;(define (tock s)
    (... (fn-for-loinvader (game-invaders s))
         (fn-for-lom (game-missiles s))
         (fn-for-tank (game-tank s))))

;(define (tock s) s)             ;stub
(define (tock s)
  (make-game (advance-invaders s) (advance-missiles s) (advance-tank (game-tank s))))

;; Game -> ListOfInvader
;; move all invanders in the list, then remove off screen and hit ones and add new random invader
(check-expect (advance-invaders G0) empty)
(check-expect (advance-invaders (make-game (list I1) empty T0)) (list (make-invader (+ (invader-x I1) INVADER-X-SPEED) (+ (invader-y I1) INVADER-Y-SPEED) (invader-dx I1))))

;(define (advance-invaders s) (game-invaders s))  ;stub
(define (advance-invaders s) (add-invader INVADE-RATE (remove-hit (game-missiles s) (move-invaders (game-invaders s)))))

;; ListOfInavders -> ListOfInvaders
;; Move all in the list
(check-expect (move-invaders empty) empty)
(check-expect (move-invaders (list I1)) (list (advance-invader I1)))

;(define (move-invaders loi) loi)  ;stub
(define (move-invaders loi)
  (cond [(empty? loi) empty]
        [else (cons (advance-invader (first loi)) (move-invaders (rest loi)))]))

;; Invader -> Invader
;; Set direction and move invader
(check-expect (advance-invader I1) (move-invader I1))

;(define (advance-invader i) i)  ;stub
(define (advance-invader i) (move-invader (set-direction i)))

;; Invader -> Invader
;; Inver dx of invader if he hits the edge
(check-expect (set-direction I1) I1)
(check-expect (set-direction (make-invader WIDTH 100 10)) (make-invader WIDTH 100 -10))
(check-expect (set-direction (make-invader WIDTH 100 -10)) (make-invader WIDTH 100 -10))
(check-expect (set-direction (make-invader 0 100 10)) (make-invader 0 100 10))
(check-expect (set-direction (make-invader 0 100 -10)) (make-invader 0 100 10))

;(define (set-direction i) i)  ;stub
(define (set-direction i)
  (cond [(and (<= (invader-x i) 0) (< (invader-dx i) 0)) (make-invader (invader-x i) (invader-y i) (* (invader-dx i) -1))]
        [(and (>= (invader-x i) WIDTH) (> (invader-dx i) 0)) (make-invader (invader-x i) (invader-y i) (* (invader-dx i) -1))]
        [else i]))

;; Invader -> Invader
;; Change invaders x and y coords according to dx
(check-expect (move-invader I1) (make-invader (+ (invader-x I1) INVADER-X-SPEED) (+ (invader-y I1) INVADER-Y-SPEED) (invader-dx I1)))
(check-expect (move-invader (make-invader 100 100 -10)) (make-invader (- 100 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) -10))

;(define (move-invader i) i)  ;stub
(define (move-invader i)
  (if (> (invader-dx i) 0)
      (make-invader (+ (invader-x i) INVADER-X-SPEED) (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i))
      (make-invader (- (invader-x i) INVADER-X-SPEED) (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i))))

;; ListOfMissiles ListOfInvaders -> ListOfInvaders
;; Remove from the list invaders that were hit by missiles
(check-expect (remove-hit empty empty) empty)
(check-expect (remove-hit empty (list I1)) (list I1))
(check-expect (remove-hit (list M1) (list I1)) (list I1))
(check-expect (remove-hit (list M2) (list I1)) empty)
(check-expect (remove-hit (list M1 M2) (list I1 I2)) (list I2))


;(define (remove-hit lom loi) loi)  ;stub
(define (remove-hit lom loi)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [(hit-any-missile? lom (first loi)) (remove-hit lom (rest loi))]
        [else (cons (first loi) (remove-hit lom (rest loi)))]))


;; ListOfMissiles Invader -> Boolean
;; Produce true if invader was hit by any of missiles in the list
(check-expect (hit-any-missile? empty I1) false)
(check-expect (hit-any-missile? (list M1) I1) false)
(check-expect (hit-any-missile? (list M2) I1) true)
(check-expect (hit-any-missile? (list M1 M2) I1) true)

;(define (hit-any-missile? lom i) false)  ;stub
(define (hit-any-missile? lom i)
  (cond [(empty? lom) false]
        [(hit-one? i (first lom)) true]
        [else (hit-any-missile? (rest lom) i)]))
  
;; Natural ListOfInaders -> ListOfInavaders
;; Add invaders with some rate in the random x in [0, WIDTH] on the top of the screen
(check-random (add-invader 100 empty) empty)

;(define (add-invader n loi) loi)  ;stub
(define (add-invader n loi)
  (if (check-condition n)
      (cons (random-invader WIDTH) loi)
      loi))

;; Natural -> Inavder
;; Produce invader with random x coord at the top of the screen with random dx
(check-random (random-invader 1) (make-invader (random 1) 0 (rand-dir 0)))

;(define (random-invader x) I1)  ;stub
(define (random-invader x)
  (make-invader (random x) 0 (rand-dir 0)))

;; Natural -> Number
;; Produce 1 or -1 randomly
(check-member-of (rand-dir 0) 1 -1)
(check-random (rand-dir 0) (if (zero? (random 2))
                               -1
                               1))
                               
;(define (rand-dir n) 1)  ;stub
(define (rand-dir n)
  (if (zero? (random 2))
      -1
      1))

;; Natural -> Boolean
;; Produce true if invader needs to be added by empirical expression

(define (check-condition n)
  (> (random n) 98))


;; Game -> ListOfMissile
;; if missile is in the view field and not hit then move missile
(check-expect (advance-missiles G0) empty)
(check-expect (advance-missiles (make-game empty (list M1) T0)) (list (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED))))
(check-expect (advance-missiles (make-game (list I1) (list M2) T0)) empty)

;(define (advance-missiles s) empty)  ;stub
(define (advance-missiles s) (not-hit (game-invaders s) (onscreen-only (move-missiles (game-missiles s)))))

;; ListOfMissile -> ListOfMissile
;; Move each missile
(define (move-missiles lom)
  (cond [(empty? lom) empty]
        [else (cons (shift-missile (first lom))
                    (move-missiles (rest lom)))]))

;; Missile -> Missile
;; move missile by substracting MISSILE-SPEED from y coord
(check-expect (shift-missile M1) (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED)))

;(define (advance-missile m) M1)  ;stub
(define (shift-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; ListOfMissile -> ListOfMissile
;; Remove from list out of the screen missiles
(check-expect (onscreen-only empty) empty)
(check-expect (onscreen-only (list M1)) (list M1))
(check-expect (onscreen-only (list (make-missile (/ WIDTH 2) 0))) (list (make-missile (/ WIDTH 2) 0)))
(check-expect (onscreen-only (list (make-missile (/ WIDTH 2) -1))) empty)
(check-expect (onscreen-only (list M1 (make-missile (/ WIDTH 2) -1))) (list M1))

;(define (onscreen-only lom) lom)  ;stub
(define (onscreen-only lom)
  (cond [(empty? lom) empty]
        [(onscreen? (first lom)) (cons (first lom) (onscreen-only (rest lom)))]
        [else (onscreen-only (rest lom))]))

;; Missile -> Boolean
;; Produce true if 0<= missile y coord <= HEIGHT
(check-expect (onscreen? M1) true)
(check-expect (onscreen? (make-missile (/ WIDTH 2) 0)) true)
(check-expect (onscreen? (make-missile (/ WIDTH 2) -1)) false)
(check-expect (onscreen? (make-missile (/ WIDTH 2) HEIGHT)) true)
(check-expect (onscreen? (make-missile (/ WIDTH 2) (+ HEIGHT 1))) false)

;(define (onscreen? m) true)  ;stub
(define (onscreen? m)
  (cond [(< (missile-y m) 0) false]
        [(> (missile-y m) HEIGHT) false]
        [else true]))

;; ListOfInvader ListOfMissile -> ListOfMissile
;; Remove from the list of missiles ones that are hit invaders
(check-expect (not-hit empty empty) empty)
(check-expect (not-hit (list I1) empty) empty)
(check-expect (not-hit (list I1) (list M1)) (list M1))
(check-expect (not-hit (list I1) (list M2)) empty)
(check-expect (not-hit (list I1) (list M1 M2)) (list M1))

;(define (not-hit loi lom) lom)  ;stub
(define (not-hit loi lom)
  (cond [(empty? lom) empty]
        [(empty? loi) lom]
        [(hit-any? loi (first lom)) (not-hit loi (rest lom))]
        [else (cons (first lom) (not-hit loi (rest lom)))]))


;; ListOfInvaders Missile -> Boolean
;; Produce true if missile hits any of invaders in the list
(check-expect (hit-any? empty M1) false)
(check-expect (hit-any? (list I1) M1) false)
(check-expect (hit-any? (list I1) M2) true)
(check-expect (hit-any? (list I1 I2) M2) true)

;(define (hit-any? loi m) false)  ;stub
(define (hit-any? loi m)
  (cond [(empty? loi) false]
        [(hit-one? (first loi) m) true]
        [else (hit-any? (rest loi) m)]))

;; Invader Missile -> Boolean
;; Produce true if missile coords in hit range of invader
(check-expect (hit-one? I1 M1) false)
(check-expect (hit-one? I1 M2) true)
(check-expect (hit-one? I1 (make-missile (- (invader-x I1) 7) (- (invader-y I1) 7))) true)

;(define (hit-one? i m) false)  ;stub
(define (hit-one? i m)
  (cond [(< (missile-x m) (- (invader-x i) HIT-RANGE)) false]
        [(> (missile-x m) (+ (invader-x i) HIT-RANGE)) false]
        [(< (missile-y m) (- (invader-y i) HIT-RANGE)) false]
        [(> (missile-y m) (+ (invader-y i) HIT-RANGE)) false]
        [else true]))


;; Tank -> Tank
;; If tank is on the edge return tank, else move tank
(check-expect (advance-tank T0) (move-tank T0))
(check-expect (advance-tank (make-tank WIDTH 1)) (make-tank WIDTH 1))
(check-expect (advance-tank (make-tank WIDTH -1)) (move-tank (make-tank WIDTH -1)))

;(define (advance-tank t) T0)  ; stub
(define (advance-tank t)
  (cond [(and (= (tank-x t) 0) (= (tank-dir t) -1)) t]
        [(and (= (tank-x t) WIDTH) (= (tank-dir t) 1)) t]
        [else (move-tank t)]))

;; Tank -> Tank
;; move tank to TANK-SPEED pixels in the tank-dir
(check-expect (move-tank T0) (make-tank (+ (tank-x T0) TANK-SPEED) (tank-dir T0)))
(check-expect (move-tank T2) (make-tank (- (tank-x T2) TANK-SPEED) (tank-dir T2)))

;(define (move-tank t) T0)  ;stub
(define (move-tank t)
  (if (= (tank-dir t) 1)
      (make-tank (+ (tank-x t) TANK-SPEED) (tank-dir t))
      (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t))))


;; Game -> Image
;; render current game state 
#;; template from game definition
(define (tock s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))

(check-expect (render G0) (overlay (render-loinvader (game-invaders G0))
                                   (render-lom (game-missiles G0))
                                   (render-tank (game-tank G0))))
                                   

; (define (render s) BACKGROUND)   ;stub
(define (render s)
  (overlay (render-loinvader (game-invaders s))
           (render-lom (game-missiles s))
           (render-tank (game-tank s))))

;; ListOfInvader -> Image
;; render list of invaders

(check-expect (render-loinvader empty) TRANSP-BG)
(check-expect (render-loinvader (list I1)) (place-image INVADER
                                                        (invader-x I1)
                                                        (invader-y I1)
                                                        (render-loinvader empty)))
(check-expect (render-loinvader (list I1 I2)) (place-image INVADER
                                                           (invader-x I1)
                                                           (invader-y I1)
                                                           (place-image INVADER
                                                                        (invader-x I2)
                                                                        (invader-y I2)
                                                                        (render-loinvader empty))))

;(define (render-loinvader loi) BACKGROUND)  ;stub
(define (render-loinvader loi)
  (cond [(empty? loi) TRANSP-BG]
        [else (render-invader (first loi) (render-loinvader (rest loi)))]))

;; Invader Image -> Image
;; place image of invader on some background
(check-expect (render-invader I1 TRANSP-BG) (place-image INVADER
                                                         (invader-x I1)
                                                         (invader-y I1)
                                                         TRANSP-BG)) 

;(define (render-invader i bg) (square 1 "solid" "red"))  ;stub
(define (render-invader i bg)
  (place-image INVADER
               (invader-x i)
               (invader-y i)
               bg))


;; ListOfMissile -> Image
;; render list of missiles
(check-expect (render-lom empty) TRANSP-BG)
(check-expect (render-lom (list M1)) (place-image MISSILE
                                                  (missile-x M1)
                                                  (missile-y M1)
                                                  (render-lom empty)))
(check-expect (render-lom (list M1 M2)) (place-image MISSILE
                                                     (missile-x M2)
                                                     (missile-y M2)
                                                     (render-lom (list M1))))
              

;(define (render-lom lom) BACKGROUND)  ;stub
(define (render-lom lom)
  (cond [(empty? lom) TRANSP-BG]
        [else (render-missile (first lom) (render-lom (rest lom)))]))

;; Missile Image -> Image
;; place image of missile on some background
(check-expect (render-missile M1 BACKGROUND) (place-image MISSILE
                                                          (missile-x M1)
                                                          (missile-y M1)
                                                          BACKGROUND)) 


;(define (render-missile m bg) (square 1 "solid" "green"))
(define (render-missile m bg)
  (place-image MISSILE
               (missile-x m)
               (missile-y m)
               bg))


;; Tank -> Image
;; produce image with tank placed in tank x coord on background
(check-expect (render-tank T0) (place-image TANK
                                            (tank-x T0)
                                            (- HEIGHT TANK-HEIGHT/2)
                                            BACKGROUND))
(check-expect (render-tank (make-tank WIDTH -1)) (place-image TANK
                                                              WIDTH
                                                              (- HEIGHT TANK-HEIGHT/2)
                                                              BACKGROUND))

;(define (render-tank t) (square 1 "solid" "white"))  ;stub
(define (render-tank t) (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))


;; Game -> Boolean
;; Produce true if any invader in list hits the bottom otherwise false
(check-expect (invader-bottom? G0) false)
(check-expect (invader-bottom? (make-game (list I1) empty T0)) false)
(check-expect (invader-bottom? (make-game (list I2) empty T0)) true)
(check-expect (invader-bottom? (make-game (list I1 I2) empty T0)) true)
(check-expect (invader-bottom? (make-game (list I1 (make-invader 150 150 1)) empty T0)) false)

;(define (invader-bottom? s) false)  ;stub
(define (invader-bottom? s) (check-invaders (game-invaders s)))

;; ListOfInvader -> Boolean
;; Produce true if any invader in list hits the bottom otherwise false
(check-expect (check-invaders empty) false)
(check-expect (check-invaders (list I1)) false)
(check-expect (check-invaders (list I2)) true)
(check-expect (check-invaders (list I1 I2)) true)

;(define (check-invaders loi) false)  ;stub
(define (check-invaders loi)
  (cond [(empty? loi) false]
        [(invader-down? (first loi)) true]
        [else (check-invaders (rest loi))]))

;; Invader -> Boolean
;; Produce true if invader y coord is >= HEIGHT
(check-expect (invader-down? I1) false)
(check-expect (invader-down? I2) true)
(check-expect (invader-down? I3) true)

;(define (invader-down? i) false)  ;stub
(define (invader-down? i)
  (>= (invader-y i) HEIGHT))
  


;; KeyEvent Game -> Game
;; Call related function for keys
;; template
#;(define (handle-key ws ke)
  (cond [(key=? ke " ") (... ws)]
        [else 
         (... ws)]))

;(define (handle-key ke s) G1)  ;stub
(define (handle-key s ke)
  (cond [(key=? ke "left") (left-key s)]
        [(key=? ke "right") (right-key s)]
        [(key=? ke " ") (space-key s)]
        [else s]))


;; Game -> Game
;; left key pressed handler
(check-expect (left-key G0) (make-game (game-invaders G0) (game-missiles G0) (tank-left T0)))

;(define (left-key s) G2)   ;stub
(define (left-key s) (make-game (game-invaders s) (game-missiles s) (tank-left (game-tank s))))

;; Tank -> Tank
;; set tank direction to left
(check-expect (tank-left T0) (make-tank (tank-x T0) -1))
(check-expect (tank-left T2) (make-tank (tank-x T2) -1))

;(define (tank-left t) t)  ;stub
(define (tank-left t)
  (if (= (tank-dir t) -1)
      t
      (make-tank (tank-x t) -1)))


;; Game -> Game
;; right key pressed handler
(check-expect (right-key G1) (make-game (game-invaders G1) (game-missiles G1) (tank-right T1)))

;(define (right-key s) G3)  ;stub
(define (right-key s) (make-game (game-invaders s) (game-missiles s) (tank-right (game-tank s))))

;; Tank -> Tank
;; set tank direction to right
(check-expect (tank-right T0) (make-tank (tank-x T0) 1))
(check-expect (tank-right T2) (make-tank (tank-x T2) 1))

;(define (tank-right t) t)  ;stub
(define (tank-right t)
  (if (= (tank-dir t) 1)
      t
      (make-tank (tank-x t) 1)))


;; Game -> Game
;; space key pressed handler
(check-expect (space-key G0) (make-game (game-invaders G0) (launch-missile G0) (game-tank G0)))

;(define (space-key s) G1)  ;stub
(define (space-key s)
  (make-game (game-invaders s) (launch-missile s) (game-tank s)))

;; Game -> Game
;; create new missile in current tank pos and add it to the list
(check-expect (launch-missile G0) (add-missile (shoot (game-tank G0)) (game-missiles G0)))

;(define (launch-missile s) G0)  ;stub
(define (launch-missile s)
  (add-missile (shoot (game-tank s)) (game-missiles s)))

;; Tank -> Missile
;; create new missile in current tank pos
(check-expect (shoot T0) (make-missile (tank-x T0) (- HEIGHT TANK-HEIGHT/2)))
(check-expect (shoot T1) (make-missile (tank-x T1) (- HEIGHT TANK-HEIGHT/2)))

;(define (shoot t) M1)   ;stub
(define (shoot t)
  (make-missile (tank-x t) (- HEIGHT TANK-HEIGHT/2)))


;; Missile ListOfMissile -> ListOfMissile
;; Add new missile to the list
(check-expect (add-missile M1 empty) (list M1))
(check-expect (add-missile M3 (list M1 M2)) (list M3 M1 M2))

;(define (add-missile m lom) empty)   ;stub
(define (add-missile m lom)
  (cond [(empty? lom) (cons m empty)]
        [else (cons m (add-missile (first lom) (rest lom)))]))