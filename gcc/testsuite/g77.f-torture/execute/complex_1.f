      program complex_1
      complex      z0, z1, z2

      z0 = cmplx(0.,.5)
      z1 = 1./z0
      if (z1 .ne. cmplx(0.,-2)) call abort

      z0 = 10.*z0
      if (z0 .ne. cmplx(0.,5.)) call abort

      z2 = cmplx(1.,2.)
      z1 = z0/z2
      if (z1 .ne. cmplx(2.,1.)) call abort

      z1 = z0*z2
      if (z1 .ne. cmplx(-10.,5.)) call abort
      end

