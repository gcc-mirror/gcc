c { dg-do compile }
      subroutine geo2()
      implicit none

      integer ms,n,ne(2)

      ne(1) = 1
      ne(2) = 2
      ms = 1

      call call_me(ne(1)*ne(1))

      n = ne(ms)
      end
