! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR fortran/33197
!
! Fortran 2008 complex trigonometric functions: tan, cosh, sinh, tanh
!
real :: r
complex :: z
r = -45.5
r = sin(r)
r = cos(r)
r = tan(r)
r = cosh(r)
r = sinh(r)
r = tanh(r)
z = 4.0
z = cos(z)
z = sin(z)
z = tan(z) ! { dg-error "Fortran 2008: COMPLEX argument" }
z = cosh(z)! { dg-error "Fortran 2008: COMPLEX argument" }
z = sinh(z)! { dg-error "Fortran 2008: COMPLEX argument" }
z = tanh(z)! { dg-error "Fortran 2008: COMPLEX argument" }
end
