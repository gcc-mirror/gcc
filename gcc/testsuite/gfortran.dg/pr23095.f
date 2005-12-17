      ! { dg-do compile { target i?86-*-* x86_64-*-* } }
      ! { dg-options "-w -m32 -O2 -ffloat-store -fgcse-after-reload" }
      !
      ! GCSE after reload made a stack register live across an abnormal
      ! edges for one of the computed jumps.  This bombed in reg-stack.
      function foo(n) 
      real(kind=8) foo 
      integer ix, n, next 
      real(kind=8) xmax, absx 
      foo  = 0.0d0 
      assign 20 to next 
      do ix = 1,n 
         go to next,(10, 30) 
   10    assign 40 to next 
         go to 40 
   20    if (absx .gt. 8.232d-11) go to 40 
   30    if (absx .le. xmax) go to 40 
         xmax = absx 
   40    go to next,(10, 30) 
      end do 
      return 
      end 
