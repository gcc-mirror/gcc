* g77 0.5.23 and previous had bugs involving too little space
* allocated for EQUIVALENCE and COMMON areas needing initial
* padding to meet alignment requirements of the system.

      call subr
      end

      subroutine subr
      implicit none
      save

      real r1(5), r2(5), r3(5)
      double precision d1, d2, d3
      integer i1, i2, i3
      equivalence (d1, r1(2))
      equivalence (d2, r2(2))
      equivalence (d3, r3(2))

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

      if (r1(1) .ne. 1.) call abort
      if (d1 .ne. 10.) call abort
      if (r1(4) .ne. 1.) call abort
      if (r1(5) .ne. 1.) call abort
      if (i1 .ne. 1) call abort
      if (r2(1) .ne. 2.) call abort
      if (d2 .ne. 20.) call abort
      if (r2(4) .ne. 2.) call abort
      if (r2(5) .ne. 2.) call abort
      if (i2 .ne. 2) call abort
      if (r3(1) .ne. 3.) call abort
      if (d3 .ne. 30.) call abort
      if (r3(4) .ne. 3.) call abort
      if (r3(5) .ne. 3.) call abort
      if (i3 .ne. 3) call abort

      end

