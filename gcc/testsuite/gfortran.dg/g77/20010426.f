c { dg-do compile }
      function f(c)
      implicit none
      real(kind=8) c, f
      f = sqrt(c)
      return
      end
