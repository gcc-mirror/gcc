c { dg-do run { xfail *-*-linux-gnu } }
c Fix PR libgfortran/24313
      complex x, y
      complex z
      z = cmplx(0.707106, -0.707106)
      x = cmplx(0.0,-1.0)
      y = sqrt(x)
      if (abs(y - z) / abs(z) > 1.e-4) call abort

      x = cmplx(tiny(1.),-1.0)
      y = sqrt(x)
      if (abs(y - z) / abs(z) > 1.e-4) call abort

      x = cmplx(-tiny(1.),-1.0)
      y = sqrt(x)
      if (abs(y - z) / abs(z) > 1.e-4) call abort

      end

