! { dg-do compile }
! { dg-additional-options "-O3" }

SUBROUTINE MY_ROUTINE (N, A, B )
IMPLICIT NONE
INTEGER,   INTENT(IN)    :: N
COMPLEX,   INTENT(IN)    :: A(N)
COMPLEX,   INTENT(OUT)   :: B(N)
INTEGER                  :: II
B(:) = (1.,0.)
DO II = 1, N-1
    B(II) = A(N-II+1) / A(N-II)
ENDDO
END SUBROUTINE MY_ROUTINE
