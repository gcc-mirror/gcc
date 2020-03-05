! { dg-do compile }
! { dg-additional-options "-Wdo-subscript" }
! PR 90563 - this used to be rejected, wrongly
! Original test case by Tobias Neumann
program test
      implicit none
      integer, parameter :: swap(4) = [2,1,3,4]
      real :: p(20)
      integer :: j

      p = 0.0

      ! The following warnings are actually bogus, but we are not yet
      ! clever enough to suppress them.
      do j=1,6 ! { dg-warning "out of bounds" }
          if (j<5) then
              p(j) = p(swap(j)) ! { dg-warning "out of bounds" }
          endif
      enddo
end program
