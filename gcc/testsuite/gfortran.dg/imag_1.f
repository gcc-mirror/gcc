! { dg-do compile }
      program bug
      implicit none
      double complex z
      double precision x,y
      z = cmplx(1.e0_8,2.e0_8)
      y = imag(z)
      y = imagpart(z)
      x = realpart(z)
      end

