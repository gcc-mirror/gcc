! { dg-additional-options "-Ofast -floop-parallelize-all" }

      subroutine subsm ( n, x, xp, xx)
      integer        n, m, x(n),xp(n), xx(n), gg(n), dd_p
      do 55 i=1, n
         dd_p  = dd_p + (x(i) - xx(i))*gg(i)
 55   continue
      if ( dd_p .gt. 0  ) then
         call dcopy( n, xp, 1, x, 1 )
      endif
      end
