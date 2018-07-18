! { dg-options "-Os -fno-asynchronous-unwind-tables" }
! { dg-do run }

      real r1(5), r2(5), r3(5)
      real s1(2), s2(2), s3(2)
      double precision d1, d2, d3
      equivalence (r3, s3(2))
      equivalence (d3, r3(2))
      s1(1) = 1.
      s3(1) = 3.
      r3(1) = 3.
      d3 = 30.
      i3 = 3
      call x (s1, r1, d1, i1, s2, r2, d2, i2, s3, r3, d3, i3)
      end
      subroutine x (s1, r1, d1, i1, s2, r2, d2, i2, s3, r3, d3, i3)
      real r1(5), r2(5), r3(5)
      real s1(2), s2(2), s3(2)
      double precision d1, d2, d3
      if (s1(1) .ne. 1.) STOP 1
      if (s3(1) .ne. 3.) STOP 2
      if (r3(1) .ne. 3.) STOP 3
      if (d3 .ne. 30.) STOP 4
      if (i3 .ne. 3) STOP 5
      end
