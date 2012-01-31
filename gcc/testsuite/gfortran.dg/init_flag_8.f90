! { dg-do compile }
! { dg-options "-fno-automatic -finit-local-zero" }
!
! PR fortran/51800
!
! Contributed by Mario Baumann
!
      SUBROUTINE FOO( N, A )
      IMPLICIT NONE
      INTEGER :: N
      INTEGER :: A(1:N)
      INTEGER :: J
      INTEGER :: DUMMY(1:N)
      DO J=1,N
         DUMMY(J) = 0
         A(J) = DUMMY(J)
      END DO 
      END SUBROUTINE FOO
