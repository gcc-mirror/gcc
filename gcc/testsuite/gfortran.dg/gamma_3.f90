! { dg-do compile }
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
real :: x

x = gamma(cmplx(1.0,0.0))            ! { dg-error "is not consistent with a specific intrinsic interface" }
x = dgamma(cmplx(1.0,0.0,kind(0d0))) ! { dg-error "must be REAL" }
x = gamma(int(1))                    ! { dg-error "is not consistent with a specific intrinsic interface" }
x = dgamma(int(1))                   ! { dg-error "must be REAL" }

x = lgamma(cmplx(1.0,0.0))           ! { dg-error "must be REAL" }
x = algama(cmplx(1.0,0.0))           ! { dg-error "must be REAL" }
x = dlgama(cmplx(1.0,0.0,kind(0d0))) ! { dg-error "must be REAL" }

x = lgamma(int(1))                   ! { dg-error "must be REAL" }
x = algama(int(1))                   ! { dg-error "must be REAL" }
x = dlgama(int(1))                   ! { dg-error "must be REAL" }
end program gamma_test

