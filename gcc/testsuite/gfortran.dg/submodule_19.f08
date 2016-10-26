! { dg-do compile }
!
! Tests the fix for PR78108 in which an error was triggered by the
! generic operator being resolved more than once in submodules. This
! test checks that the error is triggered when the specific procedure
! really is inserted more than once in the interface.
!
! Note that adding the extra interface to the module produces two
! errors; the one below and 'Duplicate EXTERNAL attribute specified at (1)'
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
  interface operator (+)
    integer module function add(lhs,rhs)
      implicit none
      class(foo), intent(in) :: lhs,rhs
    end function    ! { dg-error "is already present in the interface" }
  end interface
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
  if (a + b .ne. 141) call abort
  if (a * b .ne. 4158) call abort
end
