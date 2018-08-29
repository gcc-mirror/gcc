c { dg-do run }
* g77 0.5.23 and previous had bugs involving too little space
* allocated for EQUIVALENCE and COMMON areas needing initial
* padding to meet alignment requirements of the system.

      call subr
      end

      subroutine subr
      implicit none

      real r1(5), r2(5), r3(5)
      double precision d1, d2, d3
      integer i1, i2, i3
      equivalence (r1(2), d1)
      equivalence (r2(2), d2)
      equivalence (r3(2), d3)

      r1(1) = 1.
      d1 = 10.
      r1(4) = 1.
      r1(5) = 1.
      i1 = 1
      r2(1) = 2.
      d2 = 20.
      r2(4) = 2.
      r2(5) = 2.
      i2 = 2
      r3(1) = 3.
      d3 = 30.
      r3(4) = 3.
      r3(5) = 3.
      i3 = 3

      call x (r1, d1, i1, r2, d2, i2, r3, d3, i3)

      end

      subroutine x (r1, d1, i1, r2, d2, i2, r3, d3, i3)
      implicit none

      real r1(5), r2(5), r3(5)
      double precision d1, d2, d3
      integer i1, i2, i3

      if (r1(1) .ne. 1.) STOP 1
      if (d1 .ne. 10.) STOP 2
      if (r1(4) .ne. 1.) STOP 3
      if (r1(5) .ne. 1.) STOP 4
      if (i1 .ne. 1) STOP 5
      if (r2(1) .ne. 2.) STOP 6
      if (d2 .ne. 20.) STOP 7
      if (r2(4) .ne. 2.) STOP 8
      if (r2(5) .ne. 2.) STOP 9
      if (i2 .ne. 2) STOP 10
      if (r3(1) .ne. 3.) STOP 11
      if (d3 .ne. 30.) STOP 12
      if (r3(4) .ne. 3.) STOP 13
      if (r3(5) .ne. 3.) STOP 14
      if (i3 .ne. 3) STOP 15

      end
