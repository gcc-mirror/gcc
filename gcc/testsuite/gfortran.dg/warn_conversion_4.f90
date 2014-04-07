! { dg-do compile }
! { dg-options "-Wconversion" }
!
! PR fortran/54234
!
!
module fft_mod
  implicit none
  integer, parameter :: dp=kind(0.0d0)
contains
  subroutine test
    integer :: x
    x = int (abs (cmplx(2.3,0.1)))
    x = int (abs (cmplx(2.3_dp,0.1))) ! { dg-warning "Conversion from REAL.8. to default-kind COMPLEX.4. at .1. might lose precision, consider using the KIND argument" }
    x = int (abs (cmplx(2.3,0.1_dp))) ! { dg-warning "Conversion from REAL.8. to default-kind COMPLEX.4. at .1. might lose precision, consider using the KIND argument" }
    x = int (abs (cmplx(2.3_dp,0.1_dp))) ! { dg-warning "Conversion from REAL.8. to default-kind COMPLEX.4. at .1. might lose precision, consider using the KIND argument" }
  end subroutine test
end module fft_mod
