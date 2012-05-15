! { dg-do run }
! { dg-options "-std=f2008 -fall-intrinsics" }

! PR fortran/45197
! Check that IMPURE and IMPURE ELEMENTAL in particular works.

! Contributed by Daniel Kraft, d@domob.eu.

MODULE m
  IMPLICIT NONE

  INTEGER, PARAMETER :: n = 5

  INTEGER :: i
  INTEGER :: arr(n)

CONTAINS

  ! This ought to work (without any effect).
  IMPURE SUBROUTINE foobar ()
  END SUBROUTINE foobar

  IMPURE ELEMENTAL SUBROUTINE impureSub (a)
    INTEGER, INTENT(IN) :: a

    arr(i) = a
    i = i + 1

    PRINT *, a
  END SUBROUTINE impureSub

END MODULE m

PROGRAM main
  USE :: m
  IMPLICIT NONE

  INTEGER :: a(n), b(n), s

  a = (/ (i, i = 1, n) /)

  ! Traverse in forward order.
  s = 0
  b = accumulate (a, s)
  IF (ANY (b /= (/ 1, 3, 6, 10, 15 /))) CALL abort ()

  ! And now backward.
  s = 0
  b = accumulate (a(n:1:-1), s)
  IF (ANY (b /= (/ 5, 9, 12, 14, 15 /))) CALL abort ()

  ! Use subroutine.
  i = 1
  arr = 0
  CALL impureSub (a)
  IF (ANY (arr /= a)) CALL abort ()

CONTAINS

  IMPURE ELEMENTAL FUNCTION accumulate (a, s)
    INTEGER, INTENT(IN) :: a
    INTEGER, INTENT(INOUT) :: s
    INTEGER :: accumulate
    
    s = s + a
    accumulate = s
  END FUNCTION accumulate

END PROGRAM main
