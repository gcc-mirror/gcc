! { dg-do run }
!
! PR 36322/36463
!
! Original code by James Van Buskirk.
! Modified by Janus Weil <janus@gcc.gnu.org>

module m

   use ISO_C_BINDING

   character, allocatable, save :: my_message(:)

   abstract interface
      function abs_fun(x)
         use ISO_C_BINDING
         import my_message
         integer(C_INT) x(:)
         character(size(my_message),C_CHAR) abs_fun(size(x))
      end function abs_fun
   end interface 

contains

  function foo(y)
    implicit none
    integer(C_INT) :: y(:)
    character(size(my_message),C_CHAR) :: foo(size(y))
    integer i,j
    do i=1,size(y)
      do j=1,size(my_message)
        foo(i)(j:j) = achar(iachar(my_message(j))+y(i))
      end do
    end do
  end function

  subroutine check(p,a)
    integer a(:)
    procedure(abs_fun) :: p
    character(size(my_message),C_CHAR) :: c(size(a))
    integer k,l,m
    c = p(a)
    m=iachar('a')
    do k=1,size(a)
      do l=1,size(my_message)
        if (c(k)(l:l) /= achar(m)) call abort()
        m = m + 1
      end do
    end do
  end subroutine

end module

program prog

use m

integer :: i(4) = (/0,6,12,18/)

allocate(my_message(1:6))

my_message = (/'a','b','c','d','e','f'/)

call check(foo,i)

end program
