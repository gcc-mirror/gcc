! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR fortran/33197
! PR fortran/40728
!
! Complex inverse trigonometric functions
! and complex inverse hyperbolic functions
!
! Argument type check
!

PROGRAM ArcTrigHyp
  IMPLICIT NONE
  real(4), volatile :: r4
  real(8), volatile :: r8
  complex(4), volatile :: z4
  complex(8), volatile :: z8

  r4 = 0.0_4
  r8 = 0.0_8
  z4 = cmplx(0.0_4, 0.0_4, kind=4)
  z8 = cmplx(0.0_8, 0.0_8, kind=8)

  r4 = asin(r4)
  r8 = asin(r8)
  r4 = acos(r4)
  r8 = acos(r8)
  r4 = atan(r4)
  r8 = atan(r8)

! a(sin,cos,tan)h cannot be checked as they are not part of
! Fortran 2003 - not even for real arguments

  z4 = asin(z4) ! { dg-error "Fortran 2008: COMPLEX argument" }
  z8 = asin(z8) ! { dg-error "Fortran 2008: COMPLEX argument" }
  z4 = acos(z4) ! { dg-error "Fortran 2008: COMPLEX argument" }
  z8 = acos(z8) ! { dg-error "Fortran 2008: COMPLEX argument" }
  z4 = atan(z4) ! { dg-error "Fortran 2008: COMPLEX argument" }
  z8 = atan(z8) ! { dg-error "Fortran 2008: COMPLEX argument" }
END PROGRAM ArcTrigHyp
