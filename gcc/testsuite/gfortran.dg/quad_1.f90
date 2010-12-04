! { dg-do link }
!
! This test checks whether the largest possible
! floating-point number works. That's usually
! REAL(16) -- either because the hardware supports it or
! because of libquadmath. However, it can also be
! REAL(10) or REAL(8)
!
program test_qp
   use iso_fortran_env, only: real_kinds
   implicit none
   integer, parameter :: QP = real_kinds(ubound(real_kinds,dim=1))
   real(QP), parameter :: Z1 = 1,HALF_PI = asin(Z1),PI = HALF_PI+HALF_PI
   real(QP) :: x = 0.124_QP
   complex(QP) :: z = 0.124_QP
   print *, 'kind = ', qp
   print *, x
   print *, PI
   print *, 16*atan(0.2_QP)-4*atan(Z1/239)
   print *, sin(PI)
   print *, cos(HALF_PI)
   print *, asinh(PI)
   print *, erfc(Z1)
   print *, epsilon(x)
   print *, precision(x)
   print *, digits(x)

   print *, z
   print *, PI*cmplx(0.0_qp, 1.0_qp)
! Disable the complex functions as not all "long-double" systems have
! a libm with those C99 functions. (libquadmath had), cf. PR 46584
!   print *, 16*atan(0.2_QP)-4*atan(Z1/239)
!   print *, sin(z)
!   print *, cos(z)
!   print *, sinh(z) ! asinh not implemented in libquadmath, cf. PR 46416
   print *, precision(z)
end program test_qp
