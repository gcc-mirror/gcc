! { dg-do run }
!
! PR 36322/36463
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

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

abstract interface
  function abs_fun(x,sz)
    integer,intent(in) :: x(:)
    interface
      pure integer function sz(b)
        integer,intent(in) :: b(:)
      end function
    end interface
    integer :: abs_fun(sz(x))
  end function
end interface

procedure(abs_fun) :: p

integer :: k,j(3),i(3) = (/1,2,3/)

j = p(i,mysize)

do k=1,mysize(i)
  if (j(k) /= 2*i(k)) STOP 1
end do

end

  function p(y,asz)
    implicit none
    integer,intent(in) :: y(:)
    interface
      pure integer function asz(c)
        integer,intent(in) :: c(:)
      end function
    end interface
    integer :: p(asz(y))
    integer l
    do l=1,asz(y)
      p(l) = y(l)*2
    end do
  end function
