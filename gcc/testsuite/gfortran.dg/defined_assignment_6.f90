! { dg-do compile }
!
! PR fortran/57364
!
! Contributed by Damian Rouson
!
module ref_counter_implementation
  type ref_counter
  contains
    procedure :: assign
    generic :: assignment(=) => assign
  end type
contains
  subroutine assign (lhs, rhs)
    class (ref_counter), intent(inout) :: lhs
    class (ref_counter), intent(in) :: rhs
  end subroutine
end module
module foo_parent_implementation
  use ref_counter_implementation ,only: ref_counter
  type :: foo_parent
    type(ref_counter) :: counter
  end type
contains
  type(foo_parent) function new_foo_parent()
  end function
end module
module foo_implementation
  use foo_parent_implementation ,only: foo_parent,new_foo_parent
  type, extends(foo_parent) :: foo
  end type
contains
  type(foo) function new_foo()
    new_foo%foo_parent = new_foo_parent()
 end function
end module
