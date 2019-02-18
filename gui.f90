!Kompiliert mit dem Befehl: gfortran -o gui gui.f90 -I/usr/local/dislin/gf -ldislin
!http://blog.hani-ibrahim.de/en/install-dislin-on-ubuntu.html

REAL FUNCTION funktion(fkt, x, typ) 
! ********************************************************************
  IMPLICIT NONE
  !Deklaration der Variablen für das Programm  
  INTEGER, INTENT(IN) :: fkt  !Auswahl der Funktion
  REAL   , INTENT(IN) :: x    !x-value
  INTEGER, INTENT(IN) :: typ  !0 = Stammfkt.wert 1 = Ableitungsfkt.wert
! ********************************************************************

  SELECT CASE (fkt)
    !-------------------------------------------------
    CASE (1)
      IF(typ == 0) THEN
        funktion = 5 * x**3 - 3 * x
      ELSE IF(typ == 1) THEN 
        funktion = 15 * x**2 - 3
      END IF
    !-------------------------------------------------
    CASE (2)
      IF(typ == 0) THEN 
        funktion = 63 * x**5 - 70 * x**3 + 15 * x    
      ELSE IF(typ == 1) THEN 
        funktion = 315 * x**4 - 210 * x**2 + 15  
      END IF
    !-------------------------------------------------  
    CASE (3)
      IF(typ == 0) THEN 
        funktion = x**2 - 2 
      ELSE IF(typ == 1) THEN 
        funktion = 2 * x 
      END IF
  END SELECT
END FUNCTION funktion




SUBROUTINE sekant(xOne, xTwo, typ )
! ********************************************************************
  IMPLICIT NONE
  !Deklaration der Funktionsparameter
  INTEGER, INTENT(IN)   :: typ
  REAL   , INTENT(INOUT):: xOne, xTwo
  !Deklaration der Variablen für das Programm
  REAL :: funktion, x_new = 0, fOne = 0, fTwo = 0, diff = 0, nullNenner = 0
! ********************************************************************
  
  diff = abs(xOne)
 
! Genauigkeit auf 10 Kommastellen  
  DO WHILE(diff > 0.0000000001)
    
    fOne = funktion(typ,xOne,0)
    fTwo = funktion(typ,xTwo,0) 
    nullNenner = fTwo - fOne
    
    IF(nullNenner == 0) THEN
      WRITE(*,*) 'Division by Zero, EXIT'  
      EXIT
    END IF
    
    x_new = xTwo - fTwo * REAL((xTwo-xOne)/nullNenner)    
    diff = abs(xOne-xTwo)
    
!   Wertzuweisung für nächste Iteration    
    xOne = xTwo
    xTwo = x_new  
  END DO  
  
! Plotte funktion mit Resultat
  IF(typ == 1) THEN
    CALL plot_func("Sekanten Verfahren", typ, x_new, "f(x) = 5x^3 - 3x")
  ELSE IF(typ == 2) THEN
    CALL plot_func("Sekanten Verfahren", typ, x_new, "f(x) = 63x^5 - 70x^3 + 15x")   
  ELSE IF(typ == 3) THEN
    CALL plot_func("Sekanten Verfahren", typ, x_new, "f(x) = x^2 - 2")
  END IF 
    
END SUBROUTINE




SUBROUTINE newton(x, typ)
! ********************************************************************
  IMPLICIT NONE
  !Deklaration der Variablen für das Programm  
  INTEGER, INTENT(IN):: typ
  REAL   , INTENT(INOUT) :: x   
  REAL :: funktion, diff=0, x_new = 0, f = 0, f_ = 0, input_ = 0   
! ********************************************************************

  diff = abs(x)
  
  DO WHILE(diff > 0.0000000001)
    f  = funktion(typ,x,0)
    f_ = funktion(typ,x,1)
    
    IF(f_ == 0) THEN
      WRITE(*,*) 'Bad Tangent, EXIT'  
      EXIT
    END IF
         
    x_new = x - (REAL(f)/REAL(f_))
    
    diff = abs(x-x_new)    

    x=x_new
  END DO 

! Plotte funktion mit Resultat  
  IF(typ == 1) THEN 
    CALL plot_func("Newton Verfahren", typ, x, "f(x) = 5x^3 - 3x")    
  ELSE IF(typ == 2) THEN 
    CALL plot_func("Newton Verfahren", typ, x, "f(x) = 63x^5 - 70x^3 + 15x")   
  ELSE IF(typ == 3) THEN 
    CALL plot_func("Newton Verfahren", typ, x, "f(x) = x^2 - 2")            
  END IF   
  
END SUBROUTINE


! https://www.mps.mpg.de/1757382/exa_curv
SUBROUTINE plot_func(method, func, res_point, name_title)  
! ********************************************************************
  IMPLICIT NONE
  !Deklaration der Funktionsparameter
  REAL    , INTENT(IN) :: res_point  
  INTEGER , INTENT(IN) :: func
  
  !Definiere 2 Felder für X und Y-Funktionswerte
  INTEGER, PARAMETER     :: N=5000
  REAL   , DIMENSION (N) :: XRAY,YRAY
  
  !String's
  CHARACTER(len = *), INTENT(IN) :: method, name_title
  CHARACTER(20)                  :: res_value
  
  INTEGER :: I = 0
  REAL    :: funktion, f_x = 0, bound = 0
! ********************************************************************

! Initialisiere den Plot
  CALL METAFL('CONS')
  CALL SCRMOD('REVERS')
  CALL DISINI()
  CALL PAGERA()
  CALL COMPLX()
!Bestimme Positionen des Graphen  
  CALL AXSPOS(450,1800)
!Bestimme Dimensionen / Größe des Graphen
  CALL AXSLEN(2200,1200)
!Bestimme Name für die Achsen    
  CALL NAME('X','X')
  CALL NAME('Y','Y')

  ! Definiere Überschriften
  !Speichere den Nullpunkt in ein String (13 Ziffern, 10 Kommastellen)
  Write( res_value, '(f13.10)' )  res_point
  !Schreibe Überschriften auf den Plot
  CALL TITLIN(method, 1)
  CALL TITLIN(name_title,2)
  CALL TITLIN(res_value ,3)

  ! Grenzen für den Plot definieren
  bound = abs(ceiling(res_point))

  ! Funktionswerte berechnen für N = 5000 (Punkte), Schrittweite: 0.01
  f_x = -(bound + 2)
  DO I=0,N
    XRAY(I)=(f_x)
    YRAY(I)=funktion(func, f_x, 0)
    f_x = f_x + 0.01
  END DO

  ! Konfiguriere den Plot mit den Grenzen
  CALL GRAF(-bound-1,bound+1,-bound-1, 1.0, -bound-1,bound+1,-bound-1, 1.0)

  CALL shdpat (16)
  ! Printe den Nullpunkt mit roter Farbe
  CALL color ( "RED" )  
  CALL rlcirc (res_point, 0, 0.03 )
  
  !Gitter mit schwarzer Farbe
  CALL color ( "BLACK" )
  CALL GRID(1,1)
  
  ! Überschrift mit der Farbe 'FORE'
  CALL COLOR('FORE')
  CALL TITLE()
  CALL MARKER(1)
  CALL AXGIT()
  
  !Rufe die Kurve mit blauer Farbe
  CALL COLOR('BLUE')
  CALL CURVE(XRAY,YRAY,N)
  CALL DISFIN()
END SUBROUTINE plot_func


! MAIN-STRATPUNKT
program gui
! ********************************************************************
  USE dislin  
  IMPLICIT NONE
  
  INTEGER        :: method_id = 0, method = 0, idok = 0, ip = 0, ip2=0, func_id = 0, func = 0
  REAL           :: x1 = 0, x2 = 0, zero_func = 0, res_=0
  CHARACTER *10  :: input  = "" ! String
! ********************************************************************

! Abfrage vom Verfahren    
  CALL SWGTIT("Numerische Nullstellenberechnung")
  CALL SWGWTH(40)
  CALL WGINI('VERT', ip)    
  CALL WGBOX(ip, "Newton Verfahren|Sekanten Verfahren", 1, method_id)
  CALL WGOK(ip, idok)
  CALL WGFIN
  CALL GWGBOX(method_id, method)

! Abrage der Funktion  
  CALL SWGTIT("Funktionen")
  CALL SWGWTH(40)
  CALL WGINI('VERT', ip2)
  CALL WGBOX(ip2, "5x^3 - 3x|63x^5 - 70x^3 + 15x|x^2 - 2", 1, func_id) 
  CALL WGOK(ip, idok)      
  CALL WGFIN
  CALL GWGBOX(func_id, func)
  
! Auswahl Newton, Abfrage des Startwerts  
  IF(method == 1) THEN
    CALL DWGTXT('Startwert:', input)
    READ(input,*)x1
    CALL newton(x1, func)
! Auswahl Sekanten, Abfrage der Startwerte      
  ELSE
    CALL DWGTXT('x1:', input)
    READ(input,*)x1
    input = ''
    CALL DWGTXT('x2:', input)
    READ(input,*)x2     
      CALL sekant(x1, x2, func)
  END IF
END program gui
