* =foo7.f in Burley's g77 test suite.
      subroutine x
      real a(n)
      common /foo/n
      continue
      entry y(a)
      call foo(a(1))
      end
