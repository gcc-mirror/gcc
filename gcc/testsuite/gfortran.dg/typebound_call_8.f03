! { dg-do compile }

! PR fortran/37429
! This used to ICE, check that is fixed.

MODULE touching
  IMPLICIT NONE

  TYPE :: EqnSys33
  CONTAINS
    PROCEDURE, NOPASS :: solve1
  END TYPE EqnSys33

CONTAINS

  FUNCTION solve1 ()
    IMPLICIT NONE
    REAL :: solve1(3)
    solve1 = 0.0
  END FUNCTION solve1

  SUBROUTINE fill_gap ()
    IMPLICIT NONE
    TYPE(EqnSys33) :: sys
    REAL :: res

    res = sys%solve1 () ! { dg-error "Incompatible rank" }
  END SUBROUTINE fill_gap

END MODULE touching

! { dg-final { cleanup-modules "touching" } }
