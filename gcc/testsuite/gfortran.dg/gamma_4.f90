! { dg-do run }
! { dg-require-effective-target fortran_large_real }
!
! Test the vendor intrinsic (d)gamma, lgamma and algama/dlgama
! gamma is also part of the Fortran 2008 draft; lgamma is called
! log_gamma in the Fortran 2008 draft.
!
! PR fortran/32980
!
program gamma_test
implicit none
intrinsic :: gamma, lgamma
integer, parameter :: qp = selected_real_kind(precision (0.0_8) + 1)

real(qp) :: rqp

if (abs(gamma(1.0_qp)  - 1.0_qp) > tiny(1.0_qp)) call abort()
if (abs(lgamma(1.0_qp)) > tiny(1.0_qp)) call abort()
end program gamma_test

