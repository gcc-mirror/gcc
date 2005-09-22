! { dg-do compile }
! { dg-options "-std=f95" }
      program bug
      implicit none
      double complex z
      double precision x
      z = cmplx(1.e0_8, 2.e0_8)
      x = imag(z)         ! { dg-error "has no IMPLICIT type" "" }
      x = imagpart(z)     ! { dg-error "has no IMPLICIT type" "" }
      x = realpart(z)     ! { dg-error "has no IMPLICIT type" "" }
      x = imag(x)         ! { dg-error "has no IMPLICIT type" "" }
      x = imagpart(x)     ! { dg-error "has no IMPLICIT type" "" }
      x = realpart(x)     ! { dg-error "has no IMPLICIT type" "" }
      end

