! { dg-do run }
! PR37926 - the interface did not transfer the formal
! argument list for the call to 'asz' in the specification of 'p'.
!
! Contributed by Janus Weil  <janus@gcc.gnu.org>
!
module m
contains
  pure integer function mysize(a)
    integer,intent(in) :: a(:)
    mysize = size(a)
  end function
end module

program prog
  use m
  implicit none
  character(3) :: str
  integer :: i(3) = (/1,2,3/)
  str = p(i,mysize)
  if (len(str) .ne. 3) STOP 1
  if (str .ne. "BCD") STOP 2
contains
  function p(y,asz)
    implicit none
    integer :: y(:)
    interface
      pure integer function asz(c)
        integer,intent(in) :: c(:)
      end function
    end interface
    character(asz(y)) p
    integer i
    do i=1,asz(y)
      p(i:i) = achar(iachar('A')+y(i))
    end do
  end function
end
