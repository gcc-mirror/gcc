! { dg-do compile }
!
! Clean up, made when working on PR fortran/52864
!
! Test some PURE and intent checks - related to pointers.
module m
  type t
  end type t
  integer, pointer :: x
  class(t), pointer :: y
end module m

pure subroutine foo()
  use m
  call bar(x) ! { dg-error "can not appear in a variable definition context" }
  call bar2(x) ! { dg-error "is local to a PURE procedure and has the POINTER attribute" }
  call bb(y) ! { dg-error "is local to a PURE procedure and has the POINTER attribute" }
contains
  pure subroutine bar(x)
    integer, pointer, intent(inout) :: x
  end subroutine
  pure subroutine bar2(x)
    integer, pointer :: x
  end subroutine
  pure subroutine bb(x)
    class(t), pointer, intent(in) :: x 
  end subroutine
end subroutine
