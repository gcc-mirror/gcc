! { dg-do run }
! Tests the fix for PR23446. Based on PR example.
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
! Tests furthermore the fix for PR fortran/29916.
! Test contributed by Marco Restelli <mrestelli@gmail.com>
!
PROGRAM TST
  INTEGER IMAX
  INTEGER :: A(4) = 1
  IMAX=2

  CALL S(A)
  CALL T(A)
  CALL U(A)
  if ( ALL(A.ne.(/2,2,3,4/))) STOP 1
  if ( ALL(F().ne.(/2.0,2.0/))) STOP 2

CONTAINS
  SUBROUTINE S(A)
    INTEGER A(IMAX)
    a = 2
  END SUBROUTINE S
  SUBROUTINE T(A)
    INTEGER A(3:IMAX+4)
    A(5:IMAX+4) = 3
  END SUBROUTINE T
  SUBROUTINE U(A)
    INTEGER A(2,IMAX)
    A(2,2) = 4
  END SUBROUTINE U
  FUNCTION F()
    real :: F(IMAX)
    F = 2.0
  END FUNCTION F
ENDPROGRAM TST
