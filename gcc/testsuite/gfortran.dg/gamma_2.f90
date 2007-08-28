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
intrinsic :: gamma
intrinsic :: dgamma
intrinsic :: lgamma
intrinsic :: algama
intrinsic :: dlgama

integer, parameter :: sp = kind(1.0)
integer, parameter :: dp = kind(1.0d0)

real(sp) :: rsp = 1.0_sp
real(dp) :: rdp = 1.0_dp

rsp = gamma(rsp)  ! FIXME:  "is not included in the selected standard"
rdp = gamma(rdp)  ! FIXME:  "is not included in the selected standard"
rdp = dgamma(rdp) ! { dg-error "is not included in the selected standard" }

rsp = lgamma(rsp) ! FIXME:  "is not included in the selected standard"
rdp = lgamma(rdp) ! FIXME:  "is not included in the selected standard"
rsp = algama(rsp) ! { dg-error "is not included in the selected standard" }
rdp = dlgama(rdp) ! { dg-error "is not included in the selected standard" }
end subroutine foo
end
