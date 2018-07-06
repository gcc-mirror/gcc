! { dg-do run }
!
! Tests the fix for PR78108 in which an error was
! triggered by the module procedures being added twice
! to the operator interfaces.
!
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>
!
module foo_interface
  implicit none
  type foo
    integer :: x
  contains
    procedure :: add
    generic :: operator(+) => add
    procedure :: mult
    generic :: operator(*) => mult
  end type
  interface
    integer module function add(lhs,rhs)
      implicit none
      class(foo), intent(in) :: lhs,rhs
    end function
    integer module function mult(lhs,rhs)
      implicit none
      class(foo), intent(in) :: lhs,rhs
    end function
  end interface
end module
submodule(foo_interface) foo_implementation
contains
    integer module function add(lhs,rhs)
      implicit none
      class(foo), intent(in) :: lhs,rhs
      add = lhs % x + rhs % x
    end function
    integer module function mult(lhs,rhs)
      implicit none
      class(foo), intent(in) :: lhs,rhs
      mult = lhs % x * rhs % x
    end function
end submodule

  use foo_interface
  type(foo) :: a = foo (42)
  type(foo) :: b = foo (99)
  if (a + b .ne. 141) STOP 1
  if (a * b .ne. 4158) STOP 2
end
