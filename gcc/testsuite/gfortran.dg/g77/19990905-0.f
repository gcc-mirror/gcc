c { dg-do compile }
* =foo0.f in Burley's g77 test suite.
! Used to give "Variable 'm' cannot appear" "Variable 'm' cannot appear"
! after REAL a(m,n), as described in PR 16511.
!
      subroutine sub(a)
      equivalence (m,iarray(100))
      common /info/ iarray(1000)
      equivalence (n,iarray(200))
      real a(m,n)
      a(1,1) = a(2,2)
      end
