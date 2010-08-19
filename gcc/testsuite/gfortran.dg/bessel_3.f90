! { dg-do compile }
! { dg-options "-std=f2003 -Wimplicit-procedure" }
!
! PR fortran/36158 - Transformational BESSEL_JN/YN
! PR fortran/33197 - F2008 math functions
!
IMPLICIT NONE
print *, SIN (1.0)
print *, BESSEL_J0(1.0) ! { dg-error "has no IMPLICIT type" })
print *, BESSEL_J1(1.0) ! { dg-error "has no IMPLICIT type" }
print *, BESSEL_JN(1,1.0) ! { dg-error "has no IMPLICIT type" }
print *, BESSEL_JN(1,2,1.0) ! { dg-error "has no IMPLICIT type" }

print *, BESSEL_Y0(1.0) ! { dg-error "has no IMPLICIT type" }
print *, BESSEL_Y1(1.0) ! { dg-error "has no IMPLICIT type" }
print *, BESSEL_YN(1,1.0) ! { dg-error "has no IMPLICIT type" }
print *, BESSEL_YN(1,2,1.0) ! { dg-error "has no IMPLICIT type" }
end
