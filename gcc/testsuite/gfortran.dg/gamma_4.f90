! { dg-do run }
! { dg-require-effective-target fortran_large_real }
!
! Test the Fortran 2008 intrinsics gamma and log_gamma
!
! PR fortran/32980
!
program gamma_test
implicit none
intrinsic :: gamma, log_gamma
integer, parameter :: qp = selected_real_kind(precision (0.0_8) + 1)

real(qp) :: rqp

if (abs(gamma(1.0_qp)  - 1.0_qp) > tiny(1.0_qp)) call abort()
if (abs(log_gamma(1.0_qp)) > tiny(1.0_qp)) call abort()
end program gamma_test

