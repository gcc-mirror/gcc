* =foo0.f in Burley's g77 test suite.
      subroutine sub(a)
      common /info/ iarray(1000)
      equivalence (m,iarray(100)), (n,iarray(200))
      real a(m,n)
      a(1,1) = a(2,2)
      end
