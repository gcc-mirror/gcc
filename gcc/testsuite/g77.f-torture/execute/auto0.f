* Test automatic arrays.
      program auto0
      implicit none
      integer i
      integer j0(40)
      integer j1(40)
      integer jc0(40)
      integer jc1(40)
      common /jc0/ jc0
      common /jc1/ jc1

      data j0/40*3/
      data j1/40*4/

      i = 40
      call a1 (j0, j1, i)

      do i = 1, 40
         if (j0(i) .ne. 4) call abort
         if (j1(i) .ne. 3) call abort
         if (jc0(i) .ne. 6) call abort
         if (jc1(i) .ne. 5) call abort
      end do

      end

      block data jc
      implicit none
      integer jc0(40)
      integer jc1(40)
      common /jc0/ jc0
      common /jc1/ jc1

      data jc0/40*5/
      data jc1/40*6/

      end

      subroutine a1 (j0, j1, n)
      implicit none
      integer j0(40), j1(40), n
      integer k0(n), k1(n)
      integer i
      integer jc0(40)
      integer jc1(40)
      common /jc0/ jc0
      common /jc1/ jc1

      do i = 1, 40
         j0(i) = j1(i) - j0(i)
         jc0(i) = jc1(i) - jc0(i)
      end do

      n = -1

      do i = 1, 40
         k0(i) = n
         k1(i) = n
      end do

      do i = 1, 40
         j1(i) = j1(i) + k0(i) * j0(i)
         jc1(i) = jc1(i) + k1(i) * jc0(i)
      end do

      n = 500

      do i = 1, 40
         if (k0(i) .ne. -1) call abort
         k0(i) = n
         if (k1(i) .ne. -1) call abort
         k1(i) = n
      end do

      do i = 1, 40
         j0(i) = j1(i) + j0(i)
         jc0(i) = jc1(i) + jc0(i)
      end do

      end
