;; ---Simple ArcBall in Impromptu---  Modified 41_opengl_basics.schm
;;-------------------------------------
;; Mike Ton - mike.ton@gmail.com
;; 07/27/09
;;-------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;; OPENGL GLOBALS

(define *gl* (gl:make-opengl))             ;;makes and returns an opengl context
(gl:open-opengl *gl* '(200 200 640 480))   ;;reopens window for existing opengl instance
(define *width* (car (gl:get-window-size *gl*)))  ;;get window size and returns pair
(define *height* (cdr (gl:get-window-size *gl*)))

    ;;vector vs. list - random access to each element, and is mutable; suitable to store matrices
(define *matrixPr* (objc:vector->nsdata-float (vector 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1)))
(define *matrixFinal* (objc:vector->nsdata-float (vector 1 0 0 0 0 1 0 0 0 0 1 0 0 0 -3 1)))
(define *vecStart* (vector 1.0 1.0 1.0))

(io:register-mouse-events *gl*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; OpenGL Basic Example
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define init
   (lambda ()
      (gl:clear-color *gl* 0.2 0.2 0.3 1.0)
      (gl:enable *gl* *gl:depth-test*)))

(define view
   (lambda ()
      (gl:viewport *gl* 0 0 640 480)
      (gl:matrix-mode *gl* *gl:projection*)
      (gl:load-identity *gl*)
      (glu:perspective *gl* 40.0 (/ 640 480) 1.0 10.0)
      (gl:matrix-mode *gl* *gl:modelview*)))

(define draw
   (lambda ()
      (gl:clear *gl* (io:binary-or *gl:depth-buffer-bit* *gl:color-buffer-bit*))
      (gl:load-identity *gl*)
      (glu:look-at *gl* 0 0 5 0 0 0 0 1 0)
      
      (gl:push-matrix *gl*)
      (gl:load-matrix *gl* *matrixFinal*)
      (glut:wire-cube *gl* 1.0)
      (gl:pop-matrix *gl*)

      (gl:flush *gl*)
      (callback (+ (now) 1000) 'draw)))

(init)
(view)
(draw)

;;;;;;;;;;;;;;;;;;;;;;;;; MATH UTILS

(define norm2DPoint->3DVector
   (lambda(x y)
          ;; get x y to [-1..1] range and calculate z
      (define vecX (- (* (/ x *width* ) 2.0) 1.0))
      (define vecY (- (* (/ y *height*) 2.0) 1.0))
      (define dist (sqrt (+ (* vecX vecX) (* vecY vecY))))      
      (if (> dist 1.0)
          (define vecZ 0.0)      
          (define vecZ (- 1.0 dist)))
          ;; Normalize vector
      (define lengthVec (sqrt (+ (* vecX vecX) (* vecY vecY) (* vecZ vecZ))))
          (set! vecX (/ vecX lengthVec)) 
          (set! vecY (/ vecY lengthVec))   
          (set! vecZ (/ vecZ lengthVec))
          ;; return vector   
      (list->vector (list vecX vecY vecZ))
   )
)

(define crossProd
   (lambda(vA vB)
      (define v! (lambda(v k) (vector-ref v k))) ;;shortening vector-ref call
         ;;  A x B
      (define v_X      (- (* (v! vA 1) (v! vB 2) ) (* (v! vB 1) (v! vA 2)  ))) ; X
      (define v_Y (-   (- (* (v! vA 0) (v! vB 2) ) (* (v! vB 0) (v! vA 2) )))) ; Y
      (define v_Z      (- (* (v! vA 0) (v! vB 1) ) (* (v! vB 0) (v! vA 1)  ))) ; Z
         ;; |A x B| = (sqrt (pow vec[0] 2) + (pow vec[1] 2) + (pow vec[2] 2))
      (define length_AB (sqrt (+ (* v_X v_X) (* v_Y v_Y) (* v_Z v_Z))))
      (list->vector (list (/ v_X length_AB) (/ v_Y length_AB) (/ v_Z length_AB)))
   )
)

(define dotProd
   (lambda(vA vB)
      (define v! (lambda(v k) (vector-ref v k))) ;;shortening vector-ref call
         ;; A x B = (A[0] * B[0]) + (A[1] * B[1]) + (A[2] * B[2])
      (define A*B (+ (* (v! vA 0) (v! vB 0)) (* (v! vA 1) (v! vB 1)) (* (v! vA 2) (v! vB 2))))
      (define length_A (sqrt (+ (expt (v! vA 0) 2) (expt (v! vA 1) 2) (expt (v! vA 2) 2)) ))
      (define length_B (sqrt (+ (expt (v! vB 0) 2) (expt (v! vB 1) 2) (expt (v! vB 2) 2)) ))
         ;; return rotation
      (/ A*B (* length_A length_B)) ;57.2957795
   )
)

(define Matrix3D-from-Quarternion
   (lambda(angle x y z)
      (define mag (sqrt (+ (* x x) (* y y) (* z z))))
      (if (= mag 0) 
          (begin (set! x 1) (set! y 0) (set! z 0)) 
          (begin (set! x (/ x mag)) (set! y (/ y mag)) (set! z (/ z mag))))
      (define c (cos angle))
      (define s (sin angle))
      ;; calc matrix from quartenion
      ;; rotation
      (define X0  (+ (* (* x x) (- 1 c)) c))
      (define X1  (+ (* (* x y) (- 1 c)) (* s z)))
      (define X2  (- (* (* x z) (- 1 c)) (* s y)))
      (define Y0  (- (* (* y x) (- 1 c)) (* s z)))
      (define Y1  (+ (* (* y y) (- 1 c)) c))
      (define Y2  (+ (* (* y z) (- 1 c)) (* s x)))
      (define Z0  (+ (* (* z x) (- 1 c)) (* s y)))
      (define Z1  (- (* (* z y) (- 1 c)) (* s x)))
      (define Z2  (+ (* (* z z) (- 1 c)) c))
      ;; translation
      (define mPosX  0.0)
      (define mPosY  0.0)
      (define mPosZ  0.0)    ;;move object back from z
      ;; return matrix
      (list->vector (list X0 X1 X2 0.0 Y0 Y1 Y2 0.0 Z0 Z1 Z2 0.0 mPosX mPosY mPosZ 1.0))
      ))

(define matrix-mult
   (lambda (m1 m2)
      (define m1_00 (vector-ref m1 0))
      (define m1_01 (vector-ref m1 1))
      (define m1_02 (vector-ref m1 2))
      (define m1_03 (vector-ref m1 3))
      (define m1_04 (vector-ref m1 4))
      (define m1_05 (vector-ref m1 5))
      (define m1_06 (vector-ref m1 6))
      (define m1_07 (vector-ref m1 7))
      (define m1_08 (vector-ref m1 8))
      (define m1_09 (vector-ref m1 9))
      (define m1_10 (vector-ref m1 10))
      (define m1_11 (vector-ref m1 11))
      (define m1_12 (vector-ref m1 12))
      (define m1_13 (vector-ref m1 13))
      (define m1_14 (vector-ref m1 14))
      (define m1_15 (vector-ref m1 15))
      
      (define m2_00 (vector-ref m2 0))
      (define m2_01 (vector-ref m2 1))
      (define m2_02 (vector-ref m2 2))
      (define m2_03 (vector-ref m2 3))
      (define m2_04 (vector-ref m2 4))
      (define m2_05 (vector-ref m2 5))
      (define m2_06 (vector-ref m2 6))
      (define m2_07 (vector-ref m2 7))
      (define m2_08 (vector-ref m2 8))
      (define m2_09 (vector-ref m2 9))
      (define m2_10 (vector-ref m2 10))
      (define m2_11 (vector-ref m2 11))
      (define m2_12 (vector-ref m2 12))
      (define m2_13 (vector-ref m2 13))
      (define m2_14 (vector-ref m2 14))
      (define m2_15 (vector-ref m2 15))
      
      (define m3_00 (+ (* m1_00 m2_00) (* m1_04 m2_01) (* m1_08 m2_02) (* m1_12 m2_03)))
      (define m3_01 (+ (* m1_01 m2_00) (* m1_05 m2_01) (* m1_09 m2_02) (* m1_13 m2_03)))
      (define m3_02 (+ (* m1_02 m2_00) (* m1_06 m2_01) (* m1_10 m2_02) (* m1_14 m2_03)))
      (define m3_03 (+ (* m1_03 m2_00) (* m1_07 m2_01) (* m1_11 m2_02) (* m1_15 m2_03)))
      
      (define m3_04 (+ (* m1_00 m2_04) (* m1_04 m2_05) (* m1_08 m2_06) (* m1_12 m2_07)))
      (define m3_05 (+ (* m1_01 m2_04) (* m1_05 m2_05) (* m1_09 m2_06) (* m1_13 m2_07)))
      (define m3_06 (+ (* m1_02 m2_04) (* m1_06 m2_05) (* m1_10 m2_06) (* m1_14 m2_07)))
      (define m3_07 (+ (* m1_03 m2_04) (* m1_07 m2_05) (* m1_11 m2_06) (* m1_15 m2_07)))
      
      (define m3_08 (+ (* m1_00 m2_08) (* m1_04 m2_09) (* m1_08 m2_10) (* m1_12 m2_11)))
      (define m3_09 (+ (* m1_01 m2_08) (* m1_05 m2_09) (* m1_09 m2_10) (* m1_13 m2_11)))
      (define m3_10 (+ (* m1_02 m2_08) (* m1_06 m2_09) (* m1_10 m2_10) (* m1_14 m2_11)))
      (define m3_11 (+ (* m1_03 m2_08) (* m1_07 m2_09) (* m1_11 m2_10) (* m1_15 m2_11)))
      
      (define m3_12 (+ (* m1_00 m2_12) (* m1_04 m2_13) (* m1_08 m2_14) (* m1_12 m2_15)))
      (define m3_13 (+ (* m1_01 m2_12) (* m1_05 m2_13) (* m1_09 m2_14) (* m1_13 m2_15)))
      (define m3_14 (+ (* m1_02 m2_12) (* m1_06 m2_13) (* m1_10 m2_14) (* m1_14 m2_15)))
      (define m3_15 (+ (* m1_03 m2_12) (* m1_07 m2_13) (* m1_11 m2_14) (* m1_15 m2_15)))
      
      (list->vector (list m3_00 m3_01 m3_02 m3_03 m3_04 m3_05 m3_06 m3_07 m3_08 m3_09 
                          m3_10 m3_11 m3_12 m3_13 m3_14 m3_15))
      
      ))   
  
;;;;;;;;;;;;;;;;;;;;;;;;; MOUSE EVENTS

(define (io:mouse-down x y)
   (gl:clear-color *gl* 0.7 0.4 0.0 1.0)
   (set! *vecStart* (norm2DPoint->3DVector x y))  ;;get start vector on click
)

(define (io:mouse-up)
   (gl:clear-color *gl* 0.2 0.2 0.3 1.0)
   (set! *matrixPr* (objc:nsdata->vector *matrixFinal*))
   (sys:clear-log-view)
)

(define (io:mouse-drag x y)
   (define vecEnd (norm2DPoint->3DVector x y))
   (define vecNorm (crossProd *vecStart* vecEnd))
   (define rotAngle (acos (dotProd *vecStart* vecEnd)))
   (define matrixCu (Matrix3D-from-Quarternion rotAngle
                              (vector-ref vecNorm 0)
                              (vector-ref vecNorm 1)
                              (vector-ref vecNorm 2)))
   (print vecEnd)
   (set! *matrixFinal* (objc:vector->nsdata-float (matrix-mult *matrixPr* matrixCu)))
)