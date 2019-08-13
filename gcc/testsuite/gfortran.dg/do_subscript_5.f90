! { dg-do compile }
! PR 90563 - this used to be rejected, wrongly
! Original test case by Tobias Neumann
program test
      implicit none
      integer, parameter :: swap(4) = [2,1,3,4]
      real :: p(20)
      integer :: j

      p = 0.0

      do j=1,6
          if (j<5) then
              p(j) = p(swap(j))
          endif
      enddo
end program
