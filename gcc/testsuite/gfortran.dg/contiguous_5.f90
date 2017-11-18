! { dg-do compile }
! PR 83012 - this was incorrectly rejected.
! Original test case by Neil Carlson.
module mod
  type :: foo
    integer, pointer, contiguous :: p(:)
  contains
    procedure :: dataptr
  end type
contains
  function dataptr(this) result(dp)
    class(foo), intent(in) :: this
    integer, pointer, contiguous :: dp(:)
    dp => this%p
  end function
end module

subroutine bar(x)
  use mod
  class(foo) :: x
  integer, pointer, contiguous :: p(:)
  p => x%dataptr()
end subroutine
