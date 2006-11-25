! { dg-do compile }
! Tests the fix for PR29837, in which the following valid code
! would emit an error because of mistaken INTENT; the wrong
! specific interface would be used for the comparison.
!
! Contributed by 
!
MODULE M
  IMPLICIT NONE
  INTERFACE A
    MODULE PROCEDURE A1,A2
  END INTERFACE
CONTAINS

  SUBROUTINE A2(X)
    INTEGER, INTENT(INOUT) :: X
  END SUBROUTINE A2

  SUBROUTINE A1(X,Y)
    INTEGER, INTENT(IN) :: X
    INTEGER, INTENT(OUT) :: Y
    Y=X
  END SUBROUTINE A1

  SUBROUTINE T(X)
    INTEGER, INTENT(IN) :: X(:)
    INTEGER Y
    CALL A(MAXVAL(X),Y)
  END SUBROUTINE T
END MODULE M
! { dg-final { cleanup-modules "M" } }
