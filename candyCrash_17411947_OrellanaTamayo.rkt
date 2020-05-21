#lang racket

;Constantes
(define a 1103515245)
(define c 12345)
(define m 2147483648)

;Goals (Destruir 10 candys para superar el nivel)
(define goals 10)

;Candy
(define Candy '(1 2 3 4 5 6 7))




;1: REPRESENTACION.....................................................................................
;La Representacion elegida es una lista de lista (Filas) que tiene x elementos dentro (Columna)

;CreateBoardRL                                                                            (createboardRL 8 8 5 10 2)
;funcion crear tablero con recursion Lineal, con estados pendientes.
(define (createboardRL N M candy goals seed)
  (if (board? N M candy goals seed); comprueba si los datos ingresados son correctos
      (addfilaRL N M candy seed);llama la funcion que crea filas
      null
      )
  )

;CreateBoardRC                                                                            (createboardRC 8 8 5 10 2)
;funcion crear tablero con recursion de cola, sin estados pendiente.
(define (createboardRC N M candy goals  seed)
  (if (board? N M candy goals seed); comprueba si los datos ingresados son correctos
      (addfilaRC N M (list) candy seed);llama la funcion que crea filas
      null
      )
  )




;2: CONSTRUCTORES........................................................................................................


; agrega Candys aleatorio a la lista
(define (addcolumna m candy seed)
   (random_candy seed m candy)
 )

;agrega una fila a la list(matriz) en recursion lineal
(define (addfilaRL n m candy seed)
  (if (> n 0)
     (cons (addcolumna m candy seed)(addfilaRL (- n 1) m candy (* (+ seed 2) (+ seed 1))));llama la funcion que crea Candys dentro de la fila
      null;retorna nulo para crear el tablero
      )
  )

;agrega una fila a la list(matriz) en recursion de Cola
(define (addfilaRC n m L candy seed)
  (if (> n 0)
     (addfilaRC (- n 1) m (cons (addcolumna m candy seed) L) candy (* (+ seed 2) (+ seed 1)));llama la funcion que crea crea Candys dentro de la fila
      L;retorna el tablero
      )
  )

;función random toma un xn y obtiene el xn+1 de la secuencia de números aleatorios.
(define myRandom
  (lambda
    (xn)
    (remainder (+ (* a xn) c) m)
  )
)

;Retorna una lista de candy aleatorio
(define random_candy
  (lambda (xActual cuantos maximo)
    (if (= 0 cuantos)
        null
        (let ((xNvo (myRandom xActual)))
          (cons (selectCandy (remainder xNvo maximo) Candy) (random_candy xNvo (- cuantos 1) maximo))
          )
        )
    )
  )

;Retorna un Candy aleatorio
(define (selectCandy num Candy)
  (if (= num 0)
      (car Candy)
      (selectCandy (- num 1) (cdr Candy))
      )
  )




;3: FUNCIONES DE PERTENENCIA...........................................................................................


;Comprueba que los datos ingresados en el tablero son correctos
(define (board? N M candy goals seed)
  (if (and (integer? N) (integer? M) (and (integer? candy) (<= candy 7) (> candy 4)) (integer? seed))
      #t
      #f
      )
  )
 
;comprueba las filas son correctas con dulces correctos
(define (comparacandy fila)
  (if (list? fila);comprueba si es una lista
      (if (null? fila);comprueba si es una lista vacía
          null
          (if (equal? (car fila) (buscar Candy (car fila)))
              (if (null? (cdr fila))
                  #t
                  (comparacandy (cdr fila))
                  )
              null
           )
       )
      null
    )
  )

;comprueba dulces de la Lista Candy
(define (buscar l dato)
     (if (null? l)
        null
        (if (equal? dato (car l))
            dato
            (buscar (cdr l) dato)
         )
      )
  )
  
;comprueba que los datos sean los correctos en el punto de Origen y punto Destino
(define (obtenerpunto? T F C)
  (if (and (list? T) (integer? F) (integer? C))
      #t
      #f
      )
  )




;4: SELECTORES............................................................................................................


;funcion obtiene Origen Candy                         (obtenerPorigen (createboardRC 8 8 5 10 2) 3 3)             
(define (obtenerPorigen Mo fo co)
  (if (obtenerpunto? Mo fo co)
      (obtenerColumna (obtenerFila Mo fo) co)
      null ; no es consistente con recorrido
  )
)

;funcion obtiene destino Candy                         (obtenerPdestino (createboardRC 8 8 5 10 2) 4 3)
(define (obtenerPdestino Md fd cd)
  (if (obtenerpunto? Md fd cd)
      (obtenerColumna (obtenerFila Md fd) cd)
      null ; no es consistente con recorrido
  )
)

;obtener Columna
(define (obtenerFila M f)
  (obtenerCandy M f)
)

;obtener fila
(define (obtenerColumna Fil c)
  (obtenerCandy Fil c)
)

;obtener Candy
(define (obtenerCandy D pos)
   (if (null? D)
       null
       (if (= pos 1)
           (car D)
           (obtenerCandy (cdr D) (- pos 1))
        )
    )
)

;verifica si hay 3 o mas Dulces iguales en la fila
(define (check board)
  (if (= (car board) (cadr board) (caddr board))
      (cons (car board) (cons(cadr board) (cons (caddr board) null)))
      (if (null? (cdddr board))
          null
          (check (cdr board))
          )    
      )
  )

;verifica si hay 3 o mas Dulces iguales en la columna (obtenerPorigen Mo fo co)  
(define (igularcol board fil col)
  (if (= (obtenerPorigen board fil col) (obtenerPorigen board (+ fil 1) col) (obtenerPorigen board (+ fil 2) col))
      (cons (obtenerPorigen board fil col) (cons (obtenerPorigen board (+ fil 1) col) (cons (obtenerPorigen board (+ fil 2) col) null)))
      (if (<= (+ fil 3) (length board))
          (igularcol board (+ fil 1) col)
          null
          )
      )
  )

;retorna la primera fila de Candys iguales, luego de la jugada en el tablero
(define (tableromod2 board x y pOri pDest fil)
  (if (and (= fil (car pOri)) (= fil (car pDest))) ;modifica los candy de la misma fila
      (cons (remplazaCandy x (remplazaCandy y (car board)(cadr pOri)) (cadr pDest)) (tableromod2 (cdr board) x y pOri pDest (+ fil 1)))
      (if (= fil (car pOri)) ;el candy origen es modificado por el candy destino
          (cons (remplazaCandy y (car board)(cadr pOri)) (tableromod2 (cdr board) x y pOri pDest (+ fil 1)))
          (if (= fil (car pDest)) ;el candy destino es modificado por el candy origen
              (cons (remplazaCandy x (car board)(cadr pDest)) (tableromod2 (cdr board) x y pOri pDest (+ fil 1)))
              (if (= (length board) 0)
                  null
                  (cons (car board) (tableromod2 (cdr board) x y pOri pDest (+ fil 1)))
                  )
              )
          )
      )
  )

;retorna la ubicacion (columna) de los Candy iguales
(define (checkrandcol board)
  (if (= (car board) (cadr board) (caddr board))
      (cons (cons (length board) (car board)) (cons (cons (length (cdr board)) (cadr board)) (cons (cons (length (cddr board)) (caddr board)) null)))
      (if (null? (cdddr board))
          null
          (checkrandcol (cdr board))
          )    
      )
  )

;retorna la ubicacion (fila) de los Candy............................   (checkCandies (checkearfila 8 8 5 10 2))
(define (checkrandfil board)
  (if (null? board)
      null
      (if (null? (checkrandcol (car board)))
          (checkrandfil (cdr board))
          (if (>= (length (checkrandcol (car board))) 3)
              (cons (length board) (checkrandcol (car board)))
              (checkrandfil (cdr board))
              )
          )    
      )
  )



;5: MODIFICADORES.............................................................................................................


; remplaza candy
(define (remplazaCandy elem D pos)
  (if (and (list? D) (number? pos))
      (if (null? D)
          null
          (if (= pos 1)
              (cons elem (cdr D))
              (cons (car D) (remplazaCandy  elem (cdr D) (- pos 1)))
              )
          )
      D
      )
  )

;modifica el tablero con las dos posiciones intercambiadas........ (tableromod (createboardRC 8 8 5 10 2) 5 1 (list 1 1) (list 2 1) 1)
(define (tableromod board x y pOri pDest fil)
  (display "\n")
  (if (and (= fil (car pOri)) (= fil (car pDest))) ;modifica los candy de la misma fila
      (display (remplazaCandy x (remplazaCandy y (car board)(cadr pOri)) (cadr pDest)))
      (if (= fil (car pOri)) ;el candy origen es modificado por el candy destino
          (display (remplazaCandy y (car board)(cadr pOri)))
          (if (= fil (car pDest)) ;el candy destino es modificado por el candy origen
              (display (remplazaCandy x (car board)(cadr pDest)))
              (if (null? (cdr board))
                  (display (car board))
                  (display (car board))
                  )
              )
          )
      )
  (if (null? (cdr board))
      (display "\n")      
      (tableromod (cdr board) x y pOri pDest (+ fil 1))
      )
  )

;imprime el tablero con el tablero modificado y con los nuevos dulces dentro del tablero
(define (tableromod3 board x y z posx posy posz)
  (display "\n")
  (if (and (= (length board) (car posx)) (= (length board) (car posy)) (= (length board) (car posz))) ;modifica los candy de la misma fila
      (display (remplazafila x (remplazafila y (remplazafila z (car board)(car (cadddr posz)))(caaddr posy)) (caadr posx)))
      (if (= (length board) (car posx)) ;el candy origen es modificado por el candy destino
          (display (remplazafila x (car board)(caadr posx)))
          (if (= (length board) (car posy)) ;el candy destino es modificado por el candy origen
              (display (remplazafila y (car board)(caaddr posy)))
              (if (= (length board) (car posz)) ;el candy origen es modificado por el candy destino
                  (display (remplazafila z (car board) (car (cadddr posz))))
                  (if (null? (cdr board))
                      (display (car board))
                      (display (car board))
                      )
                  )
              )
          )
      )
  (if (null? (cdr board))
      (display "\n")      
      (tableromod3 (cdr board) x y z posx posy posz)
      )
  )


;remplaza fila con dulces aleatorio
(define (remplazafila seed D pos)
  (if (and (list? D) (number? pos))
      (if (null? D)
          null
          (if (= pos (length D))
              (cons seed (cdr D))
              (cons (car D) (remplazafila  seed (cdr D) pos))
              )
          )
      D
      )
  )

  


;6: OTRAS FUNCIONES...............................................................................................................


;.........................CheckBoard...............................................       (checkBoard (createboardRC 8 8 5 10 2))
;Funcion CheckBoard, comprueba si se respetan las condiciones elementales del tablero
(define (checkBoard board)
  (if (list? board);comprueba si es una lista
      (if (null? board);comprueba si es una lista vacía
          #f
          (if (boolean? (comparacandy (car board)))
              (if (null? (cdr board))
                  #t
                  (checkBoard (cdr board))
                  )
               #f
              )
          )
      #f
      )
  )

;...............................Play..................................... (play (createboardRC 8 8 5 10 2) (list 1 3) (list 2 3) 1)
;funcion Play
(define (play board pOri pDest seed)
  (board->string board)
  (display "\n")
  (let ((x (obtenerPorigen board (car pOri) (cadr pOri)))
        (y (obtenerPdestino board (car pDest) (cadr pDest)))
        )
    (display (cons (obtenerPorigen board (car pOri) (cadr pOri)) (cons (obtenerPdestino board (car pDest) (cadr pDest)) null)))
    (display "\n")
    (tableromod board x y pOri pDest 1)
    (display "\n")
    (display "Se encontro la primera fila igual: ")
    (display (checkCandies (tableromod2 board x y pOri pDest 1)))
    (display "\n")
    (checkrandfil (tableromod2 board x y pOri pDest 1));.......ejemplo de resultado '(7 (3 . 4) (2 . 4) (1 . 4))
    (tableromod3 (tableromod2 board x y pOri pDest 1) (car(random_candy (+ (* seed 2) 3) 1 5)) (car(random_candy (+ (* seed 2) 6) 1 5)) (car(random_candy (+ (* seed 2) 9) 1 5))
                 (checkrandfil (tableromod2 board x y pOri pDest 1))
                 (checkrandfil (tableromod2 board x y pOri pDest 1))
                 (checkrandfil (tableromod2 board x y pOri pDest 1)))
   )
  )

;..........................CheckCandy.............................................     (checkCandies (createboardRC 8 8 5 10 2))
;Funcion CheckCandies verifica si hay 3 o mas Dulces iguales.
(define (checkCandies board)
  (if (null? (checkearfila board))      
      (checkearcol board 1 1)
      (checkearfila board)
      )
  )

;........................board -> string...........................................      (board->string (createboardRC 8 8 5 10 2))
;Imprime la representacion del tablero
(define (board->string board)
  (display (car board))
  (display "\n")
  (if (null? (cdr board))
      (display "\n")
      (board->string (cdr board))
      )
  )


;verifica si hay 3 o mas Dulces iguales en una fila..............................     (checkCandies (checkearfila 8 8 5 10 2))
(define (checkearfila board)
  (if (null? board)
      null
      (if (null? (check (car board)))
          (checkearfila (cdr board))
          (if (>= (length (check (car board))) 3)
              (check (car board))
              (checkearfila (cdr board))
              )
          )    
      )
  )
  
;verifica si hay 3 o mas Dulces iguales en la columna   ;....................... (checkearcol (createboardRC 8 8 5 10 1) 1 1)
(define (checkearcol board fil col)
  (if (obtenerpunto? board fil col)
      (if (<= col (length (car board)))
          (if (<= (+ fil 2) (length  board))
              (if (>= (length (igularcol board fil col)) 3)
                  (igularcol board fil col)
                  (checkearcol board (+ fil 1) col)
                  )
              (checkearcol board (- fil (- fil 1)) (+ col 1))
              )
          null
          )
      null
      )
  )























