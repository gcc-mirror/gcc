! { dg-do run }
!
! Test the fix for PR56691 comment #7 (and comment #0).
!
! Reduced from the original of Marco Restelli  <mrestelli@gmail.com>
! by Janus Weil  <janus@gcc.gnu.org>
!
module m2
  implicit none
  type :: t_stv
    real :: f1
  end type
contains
  subroutine lcb(y)
    class(t_stv), intent(in) :: y(:)
    integer :: k
    do k=1,size(y)
      if (int(y(k)%f1) .ne. k) STOP 1
    enddo
  end subroutine
end module

program test
 use m2
 implicit none

 type(t_stv), allocatable :: work(:)

  allocate(work(4))
  work(:)%f1 = (/ 1.,2.,3.,4./)

  call lcb(work)
  call lcb(work(:4)) ! Indexing used to be offset by 1.

end program
