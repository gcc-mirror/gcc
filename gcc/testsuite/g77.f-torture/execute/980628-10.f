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
      equivalence (r1, c1(2))
      equivalence (r2, c2(2))
      equivalence (r3, c3(2))

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

      if (c1(1) .ne. '1') call abort
      if (r1 .ne. 1.) call abort
      if (c1(11) .ne. '1') call abort
      if (c4 .ne. '4') call abort
      if (c2(1) .ne. '2') call abort
      if (r2 .ne. 2.) call abort
      if (c2(11) .ne. '2') call abort
      if (c5 .ne. '5') call abort
      if (c3(1) .ne. '3') call abort
      if (r3 .ne. 3.) call abort
      if (c3(11) .ne. '3') call abort
      if (c6 .ne. '6') call abort

      end

