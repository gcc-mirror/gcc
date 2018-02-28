c { dg-do run }
c { dg-options "-std=gnu" }
c
* g77 0.5.23 and previous had bugs involving too little space
* allocated for EQUIVALENCE and COMMON areas needing initial
* padding to meet alignment requirements of the system.

      call subr
      end

      subroutine subr
      implicit none
      save

      character c1(11), c2(11), c3(11)
      real r1, r2, r3
      character c4, c5, c6
      equivalence (c1(2), r1)
      equivalence (c2(2), r2)
      equivalence (c3(2), r3)

      c1(1) = '1'
      r1 = 1.
      c1(11) = '1'
      c4 = '4'
      c2(1) = '2'
      r2 = 2.
      c2(11) = '2'
      c5 = '5'
      c3(1) = '3'
      r3 = 3.
      c3(11) = '3'
      c6 = '6'

      call x (c1, r1, c2, r2, c3, r3, c4, c5, c6)

      end

      subroutine x (c1, r1, c2, r2, c3, r3, c4, c5, c6)
      implicit none

      character c1(11), c2(11), c3(11)
      real r1, r2, r3
      character c4, c5, c6

      if (c1(1) .ne. '1') STOP 1
      if (r1 .ne. 1.) STOP 2
      if (c1(11) .ne. '1') STOP 3
      if (c4 .ne. '4') STOP 4
      if (c2(1) .ne. '2') STOP 5
      if (r2 .ne. 2.) STOP 6
      if (c2(11) .ne. '2') STOP 7
      if (c5 .ne. '5') STOP 8
      if (c3(1) .ne. '3') STOP 9
      if (r3 .ne. 3.) STOP 10
      if (c3(11) .ne. '3') STOP 11
      if (c6 .ne. '6') STOP 12

      end
