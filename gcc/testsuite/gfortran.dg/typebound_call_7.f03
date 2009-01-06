! { dg-do compile }

! PR fortran/37429
! Checks for assignments from type-bound functions.

MODULE touching
  IMPLICIT NONE

  TYPE :: EqnSys33
  CONTAINS
    PROCEDURE, NOPASS :: solve1
    PROCEDURE, NOPASS :: solve2
    PROCEDURE, NOPASS :: solve3
  END TYPE EqnSys33

CONTAINS

  FUNCTION solve1 ()
    IMPLICIT NONE
    REAL :: solve1(3)
    solve1 = 0.0
  END FUNCTION solve1

  CHARACTER(len=5) FUNCTION solve2 ()
    IMPLICIT NONE
    solve2 = "hello"
  END FUNCTION solve2

  REAL FUNCTION solve3 ()
    IMPLICIT NONE
    solve3 = 4.2
  END FUNCTION solve3

  SUBROUTINE fill_gap ()
    IMPLICIT NONE
    TYPE(EqnSys33) :: sys
    REAL :: res
    REAL :: resArr(3), resSmall(2)

    res = sys%solve1 () ! { dg-error "Incompatible rank" }
    res = sys%solve2 () ! { dg-error "Can't convert" }
    resSmall = sys%solve1 () ! { dg-error "Different shape" }

    res = sys%solve3 ()
    resArr = sys%solve1 ()
  END SUBROUTINE fill_gap

END MODULE touching

! { dg-final { cleanup-modules "touching" } }
