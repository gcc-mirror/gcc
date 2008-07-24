! { dg-do compile }
! { dg-options "-std=f2003 -Wall" }
!
! Test the vendor intrinsic (d)gamma, lgamma and algama/dlgama
! gamma is also part of the Fortran 2008 draft; lgamma is called
! log_gamma in the Fortran 2008 draft.
!
! PR fortran/32980
!
subroutine foo()
intrinsic :: gamma ! { dg-error "Fortran 2008" }
intrinsic :: dgamma ! { dg-error "extension" }
intrinsic :: lgamma ! { dg-error "extension" }
intrinsic :: algama ! { dg-error "extension" }
intrinsic :: dlgama ! { dg-error "extension" }

integer, parameter :: sp = kind(1.0)
integer, parameter :: dp = kind(1.0d0)

real(sp) :: rsp = 1.0_sp
real(dp) :: rdp = 1.0_dp

rsp = gamma(rsp)
rdp = gamma(rdp)
rdp = dgamma(rdp)

rsp = lgamma(rsp)
rdp = lgamma(rdp)
rsp = algama(rsp)
rdp = dlgama(rdp)
end subroutine foo
end
