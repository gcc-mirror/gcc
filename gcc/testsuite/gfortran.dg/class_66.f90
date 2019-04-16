! { dg-do run }
!
! Test the fix for PR78641 in which an ICE occured on assignment
! of a class array constructor to a derived type array.
!
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>
!
  implicit none
  type foo
    integer :: i = 99
  end type
  type(foo) :: bar(4)
  class(foo), allocatable :: barfoo

  allocate(barfoo,source = f(11))
  bar = [f(33), [f(22), barfoo], f(1)]
  if (any (bar%i .ne. [33, 22, 11, 1])) STOP 1
  deallocate (barfoo)

contains

  function f(arg) result(foobar)
    class(foo), allocatable :: foobar
    integer :: arg
    allocate(foobar,source = foo(arg))
  end function

end program
