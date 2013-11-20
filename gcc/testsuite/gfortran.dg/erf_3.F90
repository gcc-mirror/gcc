! { dg-do run }
! { dg-options "-fno-range-check -ffree-line-length-none -O0" }
! { dg-add-options ieee }
!
! Check that simplification functions and runtime library agree on ERF,
! ERFC and ERFC_SCALED, for quadruple-precision.

program test
  implicit none

  real(kind=16) :: x16

#define CHECK(a) \
  x16 = a ; \
  call check(erf(real(a,kind=16)), erf(x16)) ; \
  call check(erfc(real(a,kind=16)), erfc(x16)) ; \
  call check(erfc_scaled(real(a,kind=16)), erfc_scaled(x16))

  CHECK(0.0)
  CHECK(0.9)
  CHECK(1.9)
  CHECK(10.)
  CHECK(11.)
  CHECK(12.)
  CHECK(13.)
  CHECK(14.)
  CHECK(49.)
  CHECK(190.)

  CHECK(-0.0)
  CHECK(-0.9)
  CHECK(-1.9)
  CHECK(-19.)
  CHECK(-190.)

contains

  subroutine check (a, b)
    real(kind=16), intent(in) :: a, b
    print *, abs(a-b) / spacing(a)
    if (abs(a - b) > 10 * spacing(a)) call abort
  end subroutine

end program test
