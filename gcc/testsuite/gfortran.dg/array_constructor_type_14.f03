! { dg-do run }
! PR fortran/27997
!
! Array constructor with typespec
! for derived types.

PROGRAM test
  IMPLICIT NONE

  TYPE foo
    INTEGER :: i
    REAL :: x
  END TYPE foo

  TYPE(foo), PARAMETER :: x = foo(42, 42.)

  TYPE(foo), DIMENSION(2) :: arr

  arr = (/ foo :: x, foo(0, 1.) /)
  IF (arr(1)%i /= 42 .OR. arr(1)%x /= 42. .OR. &
      arr(2)%i /= 0 .OR. arr(2)%x /= 1.) THEN
    STOP 1
  END IF
END PROGRAM test
