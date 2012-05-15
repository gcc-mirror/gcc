! { dg-do compile }
! PR fortran/45786 - operators were not correctly marked as public
! if the alternative form was used.
! Test case contributed by Neil Carlson.
module foo_type
  private
  public :: foo, operator(==)
  type :: foo
    integer :: bar
  end type
  interface operator(.eq.)
    module procedure eq_foo
  end interface
contains
  logical function eq_foo (a, b)
    type(foo), intent(in) :: a, b
    eq_foo = (a%bar == b%bar)
  end function
end module

 subroutine use_it (a, b)
  use foo_type
  type(foo) :: a, b
  print *, a == b
end subroutine
