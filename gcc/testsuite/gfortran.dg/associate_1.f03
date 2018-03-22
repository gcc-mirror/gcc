! { dg-do run }
! { dg-options "-std=f2003  -cpp" }

! PR fortran/38936
! Check the basic semantics of the ASSOCIATE construct.

PROGRAM main
  IMPLICIT NONE
  REAL :: a, b, c
  INTEGER, ALLOCATABLE :: arr(:)
  INTEGER :: mat(3, 3)

  TYPE :: myt
    INTEGER :: comp
  END TYPE myt

  TYPE(myt) :: tp

  a = -2.0
  b = 3.0
  c = 4.0

  ! Simple association to expressions.
  ASSOCIATE (r => SQRT (a**2 + b**2 + c**2), t => a + b)
    PRINT *, t, a, b
    IF (ABS (r - SQRT (4.0 + 9.0 + 16.0)) > 1.0e-3) STOP 1
    IF (ABS (t - a - b) > 1.0e-3) STOP 2
  END ASSOCIATE

  ! Test association to arrays.
  ALLOCATE (arr(3))
  arr = (/ 1, 2, 3 /)
  ASSOCIATE (doubled => 2 * arr, xyz => func ())
    IF (SIZE (doubled) /= SIZE (arr)) STOP 3
    IF (doubled(1) /= 2 .OR. doubled(2) /= 4 .OR. doubled(3) /= 6) &
      STOP 4

    IF (ANY (xyz /= (/ 1, 3, 5 /))) STOP 5
  END ASSOCIATE

  ! Target is vector-indexed.
  ASSOCIATE (foo => arr((/ 3, 1 /)))
    IF (LBOUND (foo, 1) /= 1 .OR. UBOUND (foo, 1) /= 2) STOP 6
    IF (foo(1) /= 3 .OR. foo(2) /= 1) STOP 7
  END ASSOCIATE

  ! Named and nested associate.
  myname: ASSOCIATE (x => a - b * c)
    ASSOCIATE (y => 2.0 * x)
      IF (ABS (y - 2.0 * (a - b * c)) > 1.0e-3) STOP 8
    END ASSOCIATE
  END ASSOCIATE myname ! Matching end-label.

  ! Correct behavior when shadowing already existing names.
  ASSOCIATE (a => 1 * b, b => 1 * a, x => 1, y => 2)
    IF (ABS (a - 3.0) > 1.0e-3 .OR. ABS (b + 2.0) > 1.0e-3) STOP 9
    ASSOCIATE (x => 1 * y, y => 1 * x)
      IF (x /= 2 .OR. y /= 1) STOP 10
    END ASSOCIATE
  END ASSOCIATE

  ! Association to variables.
  mat = 0
  mat(2, 2) = 5;
  ASSOCIATE (x => arr(2), y => mat(2:3, 1:2))
    IF (x /= 2) STOP 11
    IF (ANY (LBOUND (y) /= (/ 1, 1 /) .OR. UBOUND (y) /= (/ 2, 2 /))) &
      STOP 12
    IF (y(1, 2) /= 5) STOP 13

    x = 7
    y = 8
  END ASSOCIATE
  IF (arr(2) /= 7 .OR. ANY (mat(2:3, 1:2) /= 8)) STOP 14

  ! Association to derived type and component.
  tp = myt (1)
  ASSOCIATE (x => tp, y => tp%comp)
    IF (x%comp /= 1) STOP 15
    IF (y /= 1) STOP 16
    y = 5
    IF (x%comp /= 5) STOP 17
  END ASSOCIATE
  IF (tp%comp /= 5) STOP 18

  ! Association to character variables.
  CALL test_char (5)

CONTAINS

  FUNCTION func ()
    INTEGER :: func(3)
    func = (/ 1, 3, 5 /)
  END FUNCTION func

  ! Test association to character variable with automatic length.
  SUBROUTINE test_char (n)
    INTEGER, INTENT(IN) :: n

    CHARACTER(LEN=n) :: str

    str = "foobar"
    ASSOCIATE (my => str)
      IF (LEN (my) /= n) STOP 19
      IF (my /= "fooba") STOP 20
      my = "abcdef"
    END ASSOCIATE
    IF (str /= "abcde") STOP 21
  END SUBROUTINE test_char

END PROGRAM main
