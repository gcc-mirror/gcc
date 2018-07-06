! { dg-do run }
! { dg-options "-Warray-temporaries " }

! Check that LBOUND/UBOUND/SIZE/SHAPE of array-expressions get simplified
! in certain cases.
! There should no array-temporaries warnings pop up, as this means that
! the intrinsic call has not been properly simplified.

! Contributed by Daniel Kraft, d@domob.eu.

PROGRAM main
  IMPLICIT NONE

  ! Some explicitely shaped arrays and allocatable ones.
  INTEGER :: a(2, 3), b(0:1, 4:6)
  INTEGER, ALLOCATABLE :: x(:, :), y(:, :)

  ! Allocate to matching sizes and initialize.
  ALLOCATE (x(-1:0, -3:-1), y(11:12, 3))
  a = 0
  b = 1
  x = 2
  y = 3

  ! Run the checks.  This should be simplified without array temporaries,
  ! and additionally correct (of course).

  ! Shape of expressions known at compile-time.
  IF (ANY (LBOUND (a + b) /= 1)) STOP 1
  IF (ANY (UBOUND (2 * b) /= (/ 2, 3 /))) STOP 2
  IF (ANY (SHAPE (- b) /= (/ 2, 3 /))) STOP 3
  IF (SIZE (a ** 2) /= 6) STOP 1

  ! Shape unknown at compile-time.
  IF (ANY (LBOUND (x + y) /= 1)) STOP 4
  IF (SIZE (x ** 2) /= 6) STOP 5

  ! Unfortunately, the array-version of UBOUND and SHAPE keep generating
  ! temporary arrays for their results (not for the operation).  Thus we
  ! can not check SHAPE in this case and do UBOUND in the single-dimension
  ! version.
  IF (UBOUND (2 * y, 1) /= 2 .OR. UBOUND (2 * y, 2) /= 3) STOP 6
  !IF (ANY (SHAPE (- y) /= (/ 2, 3 /))) STOP 7
END PROGRAM main
