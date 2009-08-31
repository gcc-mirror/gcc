! { dg-do run }
! { dg-options "-fno-range-check -ffree-line-length-none " }
! { dg-add-options ieee }
!
! Check that simplification functions and runtime library agree on ERF,
! ERFC and ERFC_SCALED.

program test
  implicit none

  interface check
    procedure check_r4
    procedure check_r8
  end interface check

  real(kind=4) :: x4
  real(kind=8) :: x8

#define CHECK(a) \
  x8 = a ; x4 = a ; \
  call check(erf(real(a,kind=8)), erf(x8)) ; \
  call check(erf(real(a,kind=4)), erf(x4)) ; \
  call check(erfc(real(a,kind=8)), erfc(x8)) ; \
  call check(erfc(real(a,kind=4)), erfc(x4)) ; \
  call check(erfc_scaled(real(a,kind=8)), erfc_scaled(x8)) ; \
  call check(erfc_scaled(real(a,kind=4)), erfc_scaled(x4)) ;

  CHECK(0.0)
  CHECK(0.9)
  CHECK(1.9)
  CHECK(19.)
  CHECK(190.)

  CHECK(-0.0)
  CHECK(-0.9)
  CHECK(-1.9)
  CHECK(-19.)
  CHECK(-190.)

contains

  subroutine check_r4 (a, b)
    real(kind=4), intent(in) :: a, b
    if (abs(a - b) > 10 * spacing(a)) call abort
  end subroutine

  subroutine check_r8 (a, b)
    real(kind=8), intent(in) :: a, b
    if (abs(a - b) > 10 * spacing(a)) call abort
  end subroutine

end program test
