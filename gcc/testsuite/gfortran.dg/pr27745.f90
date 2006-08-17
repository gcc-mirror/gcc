! { dg-do compile }
! { dg-options "-O2 -ftree-loop-linear" }

SUBROUTINE BUG
  INTEGER I, J, M
  REAL V
  COMMON  A(100,100), B(100,100), M, V
  DO 200 I = 1, M
     DO 100 J = 1, M
        V = V + A(I,J)
100     CONTINUE
        B(I,I) = B(I,I) * I
200     CONTINUE
        STOP
     END
