c { dg-do compile }
c
c  g77 gave error
c  19990905-1.f: In subroutine `x':
c  19990905-1.f:15: 
c           common /foo/n
c                   1
c  19990905-1.f:18: (continued):
c           call foo(a(1))
c                2
c  Invalid declaration of or reference to symbol `foo' at (2) [initially seen at (1)]
* =foo7.f in Burley's g77 test suite.
      subroutine x
      real a(n)
      common /foo/n  ! { dg-error "is already being used as a COMMON" }
      continue
      entry y(a)
      call foo(a(1)) ! { dg-error "is already being used as a COMMON" }
      end
