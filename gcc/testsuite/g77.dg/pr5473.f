      program pr5473
c Derived from g77.f-torture/execute/intrinsic-unix-bessel.f
c Origin: David Billinghurst <David.Billinghurst@riotinto.com>
c { dg-do compile { xfail *-*-* } }
c { dg-excess-errors "Assertion failed"  { xfail *-*-* } }
      real x, a
      integer*8 m
      x = 2.0
      m = 2
      a = BESJN(m,x)
      end
