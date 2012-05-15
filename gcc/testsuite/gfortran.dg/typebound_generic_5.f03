! { dg-do run }

! Check that generic bindings targetting ELEMENTAL procedures work.

MODULE m
  IMPLICIT NONE

  TYPE :: t
  CONTAINS
    PROCEDURE, NOPASS :: double
    PROCEDURE, NOPASS :: double_here
    GENERIC :: double_it => double
    GENERIC :: double_inplace => double_here
  END TYPE t

CONTAINS

  ELEMENTAL INTEGER FUNCTION double (val)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: val
    double = 2 * val
  END FUNCTION double

  ELEMENTAL SUBROUTINE double_here (val)
    IMPLICIT NONE
    INTEGER, INTENT(INOUT) :: val
    val = 2 * val
  END SUBROUTINE double_here

END MODULE m

PROGRAM main
  USE m
  IMPLICIT NONE

  TYPE(t) :: obj
  INTEGER :: arr(42), arr2(42), arr3(42), arr4(42)
  INTEGER :: i

  arr = (/ (i, i = 1, 42) /)

  arr2 = obj%double (arr)
  arr3 = obj%double_it (arr)

  arr4 = arr
  CALL obj%double_inplace (arr4)

  IF (ANY (arr2 /= 2 * arr) .OR. &
      ANY (arr3 /= 2 * arr) .OR. &
      ANY (arr4 /= 2 * arr)) THEN
    CALL abort ()
  END IF
END PROGRAM main
