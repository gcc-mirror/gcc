c { dg-do run }
c This program tests the fix for PR22304.
c
c provided by Paul Thomas - pault@gcc.gnu.org
c
      integer a(2), b, c
      COMMON /foo/ a
      EQUIVALENCE (a(1),b), (c, a(2))
      a(1) = 101
      a(2) = 102
      call bar ()
      END

      subroutine bar ()
      integer a(2), b, c, d
      COMMON /foo/ a
      EQUIVALENCE (a(1),b), (c, a(2))
      if (b.ne.101) STOP 1
      if (c.ne.102) STOP 2
      END

