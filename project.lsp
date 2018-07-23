;==========================================================================
; Tic Tac Toe 4 x 4 Puzzle 
;==========================================================================
; * Name : Enrique Nieto Arranz
; * User : nietoenr
; * Email: nietoenr@fit.cvut.cz
;==========================================================================




;==========================================================================
;Tic-Tac-Toe Human-Playable Puzzle
;==========================================================================

;Play()
;==========================================================================
;The main method of the game

(defun Play ()
	(let (Game RndState Move (StepCnt 0))
	"This is the human-playable version of the game"
		(setf RndState (make-random-state t)
			  Game (Init_Game RndState)
		)
		(format t "~%~5TStart state ~%~%")
		(Display_Game Game)
		(loop 
			(when (Game_Over Game) (return))
			(setf Move (Choose_Move)
			   Game (Update_Game Game Move)
			   StepCnt (1+ StepCnt)
			)
			(format t "~%~5TStep ~2D   Pressed ~S ~%" StepCnt Move)
			(Display_Game Game)
		)
		(Congratulations StepCnt)
	)
)

;Init_Game()
;==========================================================================
;The initial method that generate the game board randomly

(defun Init_Game (&optional (RS (make-random-state t)))
"Returns a random initial game board"  
	(let ((res '()))
	(dotimes (i 16 res)
		(setf res (append res (list (gensymb RS))))
	))
)

;gensymb(RS)
;==========================================================================
;This method generate a X or a O randomly

(defun gensymb (RS)
	(let (X)
		(setf X (random 2 RS))
		(if (= X 0)
			'o
			'x)
	)
)

;Game_Over(G)
;==========================================================================
;This method check if all the elements in the game board are the same

(defun Game_Over (G)
"Checks whether the game reached its goal state"
	(let (x S)
		(setf x 0)
		(setf S (nth 0 G))
		(dolist (i G)
			(if (not (string= i S))
				(setf x (+ 1 x))
			)
		)
		(if (= x 0)
			T
			NIL
		)
	)
)

;Choose_Move
;==========================================================================
;This method permits the user insert new moves

(defun Choose_Move ()
	(let (move)
		(format t "Write the number of the celL of your next move (0-15):~%")
		(setf move (read))
		move
	)
)

;Update_Game (G Move)
;==========================================================================
;This method update the game board with the new move

(defun Update_Game (G Move)
"Updates the gameoard state according to the selected move"
	(cond 	((= Move 0)
				(swiele G 0)
				(swiele G 1)
				(swiele G 4)
				(swiele G 5)
			)
			
			((= Move 1)
				(swiele G 0)
				(swiele G 1)
				(swiele G 5)
			)
			
			((= Move 2)
				(swiele G 2)
				(swiele G 3)
				(swiele G 6)
			)
			
			((= Move 3)
				(swiele G 2)
				(swiele G 3)
				(swiele G 6)
				(swiele G 7)
			)
			
			((= Move 4)
				(swiele G 0)
				(swiele G 4)
				(swiele G 5)
			)
			
			((= Move 5)
				(swiele G 0)
				(swiele G 2)
				(swiele G 5)
				(swiele G 8)
			)
			
			((= Move 6)
				(swiele G 1)
				(swiele G 3)
				(swiele G 6)
				(swiele G 9)
			)
			
			((= Move 7)
				(swiele G 3)
				(swiele G 6)
				(swiele G 7)
			)
			
			((= Move 8)
				(swiele G 8)
				(swiele G 9)
				(swiele G 12)
			)
			
			((= Move 9)
				(swiele G 4)
				(swiele G 6)
				(swiele G 9)
				(swiele G 12)
			)
			
			((= Move 10)
				(swiele G 5)
				(swiele G 7)
				(swiele G 10)
				(swiele G 13)
			)
			
			((= Move 11)
				(swiele G 10)
				(swiele G 11)
				(swiele G 15)
			)
			
			((= Move 12)
				(swiele G 8)
				(swiele G 9)
				(swiele G 12)
				(swiele G 13)
			)
			
			((= Move 13)
				(swiele G 9)
				(swiele G 10)
				(swiele G 12)
				(swiele G 13)
			)
			
			((= Move 14)
				(swiele G 9)
				(swiele G 10)
				(swiele G 14)
				(swiele G 15)
			)
			
			((= Move 15)
				(swiele G 10)
				(swiele G 11)
				(swiele G 14)
				(swiele G 15)
			)
	)
	G
)


;swiele (G cell)
;==========================================================================
;This method switch one elements of a cell to the oposite one

(defun swiele (G cell)
	(if (string= (nth cell G) 'X)
		(setf (nth cell G) 'O)
		(setf (nth cell G) 'X)
	)
)

;Display_Game (G)
;==========================================================================
;This method display the game board in the console

(defun Display_Game (G)
	(format t " ~%")
	(format t " ~S | ~S | ~S | ~S " (nth 0 G) (nth 1 G) (nth 2 G) (nth 3 G))
	(format t "~%--- --- --- --- ~%")
	(format t " ~S | ~S | ~S | ~S " (nth 4 G) (nth 5 G) (nth 6 G) (nth 7 G))
	(format t "~%--- --- --- --- ~%")
	(format t " ~S | ~S | ~S | ~S " (nth 8 G) (nth 9 G) (nth 10 G) (nth 11 G))
	(format t "~%--- --- --- --- ~%")
	(format t " ~S | ~S | ~S | ~S " (nth 12 G) (nth 13 G) (nth 14 G) (nth 15 G))
	(format t " ~%")
)

;Display_Game (G)
;==========================================================================
;This method shows the congratulations message

(defun Congratulations (Steps)
	(format t "It was needed ~D to finish the game. Congratulations!" Steps)
)


;==========================================================================
; Tic-Tac-Toe Automatic Puzzle Solver
;==========================================================================

(defun Auto_Play ()
"This is the automatic problem solver"
  (let (Game RndSt Move (StepCnt 0))
    (setf RndState (make-random-state t)
          Game (Init_Game RndState))
    (format t "~%~5TStart state ~%~%")
    (Display_Game Game)
    (Auto_Play_Game Game StepCnt)))

(defun Auto_Play_Game (Game Steps)
"Solves the game and displays the resulting solution path accordingly"
)
