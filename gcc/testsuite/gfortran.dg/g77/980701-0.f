c { dg-do run }
* g77 0.5.23 and previous had bugs involving too little space
* allocated for EQUIVALENCE and COMMON areas needing initial
* padding to meet alignment requirements of the system.

      call subr
      end

      subroutine subr
      implicit none

      real r1(5), r2(5), r3(5)
      real s1(2), s2(2), s3(2)
      double precision d1, d2, d3
      integer i1, i2, i3
      equivalence (r1, s1(2))
      equivalence (d1, r1(2))
      equivalence (r2, s2(2))
      equivalence (d2, r2(2))
      equivalence (r3, s3(2))
      equivalence (d3, r3(2))

      s1(1) = 1.
      r1(1) = 1.
      d1 = 10.
      r1(4) = 1.
      r1(5) = 1.
      i1 = 1
      s2(1) = 2.
      r2(1) = 2.
      d2 = 20.
      r2(4) = 2.
      r2(5) = 2.
      i2 = 2
      s3(1) = 3.
      r3(1) = 3.
      d3 = 30.
      r3(4) = 3.
      r3(5) = 3.
      i3 = 3

      call x (s1, r1, d1, i1, s2, r2, d2, i2, s3, r3, d3, i3)

      end

      subroutine x (s1, r1, d1, i1, s2, r2, d2, i2, s3, r3, d3, i3)
      implicit none

      real r1(5), r2(5), r3(5)
      real s1(2), s2(2), s3(2)
      double precision d1, d2, d3
      integer i1, i2, i3

      if (s1(1) .ne. 1.) STOP 1
      if (r1(1) .ne. 1.) STOP 2
      if (d1 .ne. 10.) STOP 3
      if (r1(4) .ne. 1.) STOP 4
      if (r1(5) .ne. 1.) STOP 5
      if (i1 .ne. 1) STOP 6
      if (s2(1) .ne. 2.) STOP 7
      if (r2(1) .ne. 2.) STOP 8
      if (d2 .ne. 20.) STOP 9
      if (r2(4) .ne. 2.) STOP 10
      if (r2(5) .ne. 2.) STOP 11
      if (i2 .ne. 2) STOP 12
      if (s3(1) .ne. 3.) STOP 13
      if (r3(1) .ne. 3.) STOP 14
      if (d3 .ne. 30.) STOP 15
      if (r3(4) .ne. 3.) STOP 16
      if (r3(5) .ne. 3.) STOP 17
      if (i3 .ne. 3) STOP 18

      end
