! { dg-do run }
! { dg-options "-std=f2003 -fall-intrinsics" }

! PR fortran/38936
! Check the basic semantics of the ASSOCIATE construct.

PROGRAM main
  IMPLICIT NONE
  REAL :: a, b, c
  INTEGER, ALLOCATABLE :: arr(:)

  a = -2.0
  b = 3.0
  c = 4.0

  ! Simple association to expressions.
  ASSOCIATE (r => SQRT (a**2 + b**2 + c**2), t => a + b)
    PRINT *, t, a, b
    IF (ABS (r - SQRT (4.0 + 9.0 + 16.0)) > 1.0e-3) CALL abort ()
    IF (ABS (t - a - b) > 1.0e-3) CALL abort ()
  END ASSOCIATE

  ! TODO: Test association to variables when that is supported.
  ! TODO: Test association to derived types.

  ! Test association to arrays.
  ALLOCATE (arr(3))
  arr = (/ 1, 2, 3 /)
  ASSOCIATE (doubled => 2 * arr, xyz => func ())
    IF (SIZE (doubled) /= SIZE (arr)) CALL abort ()
    IF (doubled(1) /= 2 .OR. doubled(2) /= 4 .OR. doubled(3) /= 6) &
      CALL abort ()

    IF (ANY (xyz /= (/ 1, 3, 5 /))) CALL abort ()
  END ASSOCIATE

  ! Named and nested associate.
  myname: ASSOCIATE (x => a - b * c)
    ASSOCIATE (y => 2.0 * x)
      IF (ABS (y - 2.0 * (a - b * c)) > 1.0e-3) CALL abort ()
    END ASSOCIATE
  END ASSOCIATE myname ! Matching end-label.

  ! Correct behaviour when shadowing already existing names.
  ASSOCIATE (a => 1 * b, b => 1 * a, x => 1, y => 2)
    IF (ABS (a - 3.0) > 1.0e-3 .OR. ABS (b + 2.0) > 1.0e-3) CALL abort ()
    ASSOCIATE (x => 1 * y, y => 1 * x)
      IF (x /= 2 .OR. y /= 1) CALL abort ()
    END ASSOCIATE
  END ASSOCIATE

CONTAINS

  FUNCTION func ()
    INTEGER :: func(3)
    func = (/ 1, 3, 5 /)
  END FUNCTION func

END PROGRAM main
