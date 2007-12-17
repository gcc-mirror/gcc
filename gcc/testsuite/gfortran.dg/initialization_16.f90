! { dg-do compile }
! { dg-options "-std=f95 -Wall" }
!
! PR fortran/34495
!
! Check for invalid Fortran 95 initialization expressions
!
program main
  implicit none
  real, parameter :: r1 = real(33)    ! { dg-error "Fortran 2003: Function 'real' as initialization expression" } 
  real, parameter :: r2 = dble(33)    ! { dg-error "Fortran 2003: Function 'dble' as initialization expression" }
  complex, parameter :: z = cmplx(33,33)! { dg-error "Fortran 2003: Function 'cmplx' as initialization expression" }
  real, parameter :: r4 = sngl(3.d0)  ! { dg-error "Fortran 2003: Function 'sngl' as initialization expression" }
  real, parameter :: r5 = float(33)   ! { dg-error "Fortran 2003: Function 'float' as initialization expression" }
end program main
