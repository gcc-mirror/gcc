c { dg-do compile }
* =foo0.f in Burley's g77 test suite.
      subroutine sub(a)
      common /info/ iarray(1000)
      equivalence (m,iarray(100)), (n,iarray(200))
      real a(m,n) ! { dg-bogus "Variable 'm' cannot appear" "Variable 'm' cannot appear" { xfail *-*-* } } PR 16511 
      a(1,1) = a(2,2)
      end
