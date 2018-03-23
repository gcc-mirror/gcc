c { dg-do run }
      program foo
      complex(kind=8)      z0, z1, z2

      z0 = dcmplx(0.,.5)
      z1 = 1./z0
      if (z1 .ne. dcmplx(0.,-2)) STOP 1

      z0 = 10.*z0
      if (z0 .ne. dcmplx(0.,5.)) STOP 2

      z2 = cmplx(1.,2.)
      z1 = z0/z2
      if (z1 .ne. dcmplx(2.,1.)) STOP 3

      z1 = z0*z2
      if (z1 .ne. dcmplx(-10.,5.)) STOP 4
      end

