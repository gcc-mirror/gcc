! { dg-do compile }
! { dg-options "-std=f2003" }
!
! Test the fix for pr117434, in which the F2008 addition of being permitted to
! pass an external, internal or module procedure to a dummy procedure pointer
! gave the error "Expected a procedure pointer for argument ‘<arg_name>’ at (1).
!
! This testcase checks that -std=f2008 or later is required..
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module julienne_test_description_m
  implicit none

  abstract interface
    logical function test_function_i()
    end function
  end interface

  type test_description_t
    procedure(test_function_i), pointer, nopass :: test_function_
  end type

contains

  type(test_description_t) function new_test_description(test_function)
    procedure(test_function_i), intent(in), pointer :: test_function
    new_test_description%test_function_ => test_function
  end function

end module

  use julienne_test_description_m
  implicit none
  type(test_description_t) test_description

  test_description = new_test_description(test) ! { dg-error "Fortran 2008:" }

contains

  logical function test()
    test = .true.
  end function

end
