! { dg-do compile }
! PR fortran/27997
!
! Array constructor with typespec
! for derived types, failing conversion.

PROGRAM test
  IMPLICIT NONE

  TYPE foo
    INTEGER :: i
    REAL :: x
  END TYPE foo

  TYPE bar
    LOGICAL :: logos
  END TYPE bar

  TYPE(foo), PARAMETER :: x = foo(42, 42.)

  WRITE (*,*) (/ foo :: x, foo(0, 1.), bar(.TRUE.) /) ! { dg-error "convert TYPE" }
END PROGRAM test
