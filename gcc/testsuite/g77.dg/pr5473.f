      program pr5473
c Derived from g77.f-torture/execute/intrinsic-unix-bessel.f
c Origin: David Billinghurst <David.Billinghurst@riotinto.com>
c { dg-do compile }
      real x, a
      double precision dx, da
      integer*8 m
      x = 2.0
      dx = x
      m = 2
      a = BESJN(m,x) ! { dg-error "incorrect type" "incorrect type" }
      a = BESYN(m,x) ! { dg-error "incorrect type" "incorrect type" }
      da = DBESJN(m,dx) ! { dg-error "incorrect type" "incorrect type" }
      da = DBESYN(m,dx) ! { dg-error "incorrect type" "incorrect type" }
      end
