      program foo
      complex*16      z0, z1, z2

      z0 = dcmplx(0.,.5)
      z1 = 1./z0
      if (z1 .ne. dcmplx(0.,-2)) call abort

      z0 = 10.*z0
      if (z0 .ne. dcmplx(0.,5.)) call abort

      z2 = cmplx(1.,2.)
      z1 = z0/z2
      if (z1 .ne. dcmplx(2.,1.)) call abort

      z1 = z0*z2
      if (z1 .ne. dcmplx(-10.,5.)) call abort
      end

