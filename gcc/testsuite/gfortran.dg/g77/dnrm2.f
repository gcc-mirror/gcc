c { dg-do run }
c { dg-options "-fno-bounds-check" }
CCC g77 0.5.21 `Actual Bugs':
CCC   * A code-generation bug afflicts Intel x86 targets when `-O2' is
CCC     specified compiling, for example, an old version of the `DNRM2'
CCC     routine.  The x87 coprocessor stack is being somewhat mismanaged
CCC     in cases where assigned `GOTO' and `ASSIGN' are involved.
CCC
CCC     Version 0.5.21 of `g77' contains an initial effort to fix the
CCC     problem, but this effort is incomplete, and a more complete fix is
CCC     planned for the next release.

C     Currently this test fails with (at least) `-O2 -funroll-loops' on
C     i586-unknown-linux-gnulibc1.

C     (This is actually an obsolete version of dnrm2 -- consult the
c     current Netlib BLAS.)

      integer i
      double precision a(1:100), dnrm2
      do i=1,100
         a(i)=0.D0
      enddo
      if (dnrm2(100,a,1) .ne. 0.0) call abort
      end

      double precision function dnrm2 ( n, dx, incx)
      integer i, incx, ix, j, n, next
      double precision   dx(1), cutlo, cuthi, hitest, sum, xmax,zero,one
      data   zero, one /0.0d0, 1.0d0/
      data cutlo, cuthi / 8.232d-11,  1.304d19 /
      j = 0
      if(n .gt. 0 .and. incx.gt.0) go to 10
         dnrm2  = zero
         go to 300
   10 assign 30 to next ! { dg-warning "ASSIGN" "" }
      sum = zero
      i = 1
      ix = 1
   20    go to next,(30, 50, 70, 110) ! { dg-warning "Assigned GOTO" "" }
   30 if( dabs(dx(i)) .gt. cutlo) go to 85
      assign 50 to next ! { dg-warning "ASSIGN" "" }
      xmax = zero
   50 if( dx(i) .eq. zero) go to 200
      if( dabs(dx(i)) .gt. cutlo) go to 85
      assign 70 to next ! { dg-warning "ASSIGN" "" }
      go to 105
  100 continue
      ix = j
      assign 110 to next ! { dg-warning "ASSIGN" "" }
      sum = (sum / dx(i)) / dx(i)
  105 xmax = dabs(dx(i))
      go to 115
   70 if( dabs(dx(i)) .gt. cutlo ) go to 75
  110 if( dabs(dx(i)) .le. xmax ) go to 115
         sum = one + sum * (xmax / dx(i))**2
         xmax = dabs(dx(i))
         go to 200
  115 sum = sum + (dx(i)/xmax)**2
      go to 200
   75 sum = (sum * xmax) * xmax
   85 hitest = cuthi/float( n )
      do 95 j = ix,n
      if(dabs(dx(i)) .ge. hitest) go to 100
         sum = sum + dx(i)**2
         i = i + incx
   95 continue
      dnrm2 = dsqrt( sum )
      go to 300
  200 continue
      ix = ix + 1
      i = i + incx
      if( ix .le. n ) go to 20
      dnrm2 = xmax * dsqrt(sum)
  300 continue
      end
