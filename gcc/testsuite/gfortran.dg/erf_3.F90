! { dg-do run { xfail spu-*-* ia64-*-linux* } }
! { dg-options "-fno-range-check -ffree-line-length-none -O0" }
! { dg-add-options ieee }
!
! Check that simplification functions and runtime library agree on ERF,
! ERFC and ERFC_SCALED, for quadruple-precision.
!
! XFAILed for SPU targets because our library implementation of
! the double-precision erf/erfc functions is not accurate enough.
!
! XFAILed for IA64 Linux because of a glibc bug:
! http://gcc.gnu.org/bugzilla/show_bug.cgi?id=59227

program test
  use, intrinsic :: iso_fortran_env
  implicit none

  ! QP will be the largest supported real kind, possibly real(kind=16)
  integer, parameter :: qp = real_kinds(ubound(real_kinds,dim=1))
  real(kind=qp) :: x

#define CHECK(a) \
  x = a ; \
  call check(erf(real(a,kind=qp)), erf(x)) ; \
  call check(erfc(real(a,kind=qp)), erfc(x)) ; \
  call check(erfc_scaled(real(a,kind=qp)), erfc_scaled(x))

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
    real(kind=qp), intent(in) :: a, b
    print *, abs(a-b) / spacing(a)
    if (abs(a - b) > 10 * spacing(a)) call abort
  end subroutine

end program test
