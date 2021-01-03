! { dg-do compile }

module m
  implicit none
  integer :: A
  integer, parameter :: PARM = 5  ! { dg-warning "Using parameter 'parm' declared at .1. is deprecated" }
!GCC$ ATTRIBUTES  DEPRECATED :: A, foo, func, parm
contains
subroutine foo
end
integer function func()
  func = 42
end
subroutine bar
  integer :: i
  call foo    ! { dg-warning "Using subroutine 'foo' at .1. is deprecated" }
  print *, A  ! { dg-warning "Using variable 'a' at .1. is deprecated" }
  i = func()  ! { dg-warning "Using function 'func' at .1. is deprecated" }
  print *, PARM
end
  
end module m

use m  ! { dg-warning "Using parameter 'parm' declared at .1. is deprecated" }
  integer :: i
  call foo  ! { dg-warning "Using subroutine 'foo' at .1. is deprecated" }
  print *, A  ! { dg-warning "Using variable 'a' at .1. is deprecated" }
  i = func()  ! { dg-warning "Using function 'func' at .1. is deprecated" }
  print *, PARM
end
