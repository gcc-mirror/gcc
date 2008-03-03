! { dg-do run }
!
! Test the vendor intrinsic (d)gamma, lgamma and algama/dlgama
! gamma is also part of the Fortran 2008 draft; lgamma is called
! log_gamma in the Fortran 2008 draft.
!
! PR fortran/32980
!
program gamma_test
implicit none
intrinsic :: gamma, lgamma, log_gamma
integer, parameter :: sp = kind(1.0)
integer, parameter :: dp = kind(1.0d0)

real(sp) :: rsp
real(dp) :: rdp

if (abs(gamma(1.0_sp)  - 1.0_sp) > tiny(1.0_sp)) call abort()
if (abs(gamma(1.0_dp)  - 1.0_dp) > tiny(1.0_dp)) call abort()
if (abs(dgamma(1.0_dp) - 1.0_dp) > tiny(1.0_dp)) call abort()

if (abs(lgamma(1.0_sp)) > tiny(1.0_sp)) call abort()
if (abs(lgamma(1.0_dp)) > tiny(1.0_dp)) call abort()
if (abs(log_gamma(1.0_sp)) > tiny(1.0_sp)) call abort()
if (abs(log_gamma(1.0_dp)) > tiny(1.0_dp)) call abort()
if (abs(algama(1.0_sp)) > tiny(1.0_sp)) call abort()
if (abs(dlgama(1.0_dp)) > tiny(1.0_dp)) call abort()
end program gamma_test

