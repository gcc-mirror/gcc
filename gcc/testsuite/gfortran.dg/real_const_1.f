c { dg-do run }
c
c Fixed form test program for PR 17941 (signed constants with spaces)
c
      program real_const_1
      complex c0, c1, c2, c3, c4
      real rp(4), rn(4)
      parameter (c0 = (-0.5, -     0.5))
      parameter (c1 = (-     0.5, +     0.5))
      parameter (c2 = (-    0.5E2, +0.5))
      parameter (c3 = (-0.5, +     0.5E-2))
      parameter (c4 = (-     1, +     1))
      data rn /- 1.0, - 1d0, - 1.d0, - 10.d-1/
      data rp /+ 1.0, + 1d0, + 1.d0, + 10.d-1/
      real, parameter :: del = 1.e-5

      if (abs(c0 - cmplx(-0.5,-0.5)) > del) STOP 1
      if (abs(c1 - cmplx(-0.5,+0.5)) > del) STOP 2
      if (abs(c2 - cmplx(-0.5E2,+0.5)) > del) STOP 3
      if (abs(c3 - cmplx(-0.5,+0.5E-2)) > del) STOP 4
      if (abs(c4 - cmplx(-1.0,+1.0)) > del) STOP 5
      if (any (abs (rp - 1.0) > del)) STOP 6
      if (any (abs (rn + 1.0) > del)) STOP 7
      end program
