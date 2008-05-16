! { dg-do compile }
! { dg-options "-frange-check" }
! PR fortran/27997
!
! Range check on array-constructors with typespec.

PROGRAM test
  IMPLICIT NONE

  INTEGER(KIND=4) :: arr(1)
  arr = (/ INTEGER(KIND=4) :: HUGE(0_8) /) ! { dg-error "overflow converting" }
END PROGRAM test
