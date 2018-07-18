! { dg-do run }
! { dg-options "-O" }
! PR 48412 - function elimination got temporary varibles in the wrong order.
! Test case contributed by Joost VandeVondele.

INTEGER FUNCTION S1(m,ma,lx)
INTEGER :: m,ma,lx

IF (((m < 0).AND.(MODULO(ABS(ma-lx),2) == 1)).OR.&
    ((m > 0).AND.(MODULO(ABS(ma-lx),2) == 0))) THEN
   S1=1
ELSE
   S1=0
ENDIF

END FUNCTION

INTEGER :: s1
IF (S1(1,2,1).NE.0) STOP 1
END
