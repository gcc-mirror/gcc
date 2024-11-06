! { dg-do compile }
!
! Test the fix for pr117434, in which the F2008 addition of being permitted to
! pass an external, internal or module procedure to a dummy procedure pointer
! gave the error "Expected a procedure pointer for argument ‘<arg_name>’ at (1).
!
! This testcase tests that interface checking is OK in this situation.
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module julienne_test_description_m
  implicit none

  abstract interface
    logical function test_function_i(arg)
      integer, intent(in) :: arg
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

  test_description = new_test_description(test1)
  test_description = new_test_description(test2) ! { dg-error "Type mismatch in function" }
  test_description = new_test_description(test3) ! { dg-error "wrong number of arguments" }
  test_description = new_test_description(test4) ! { dg-error "Rank mismatch in argument" }
  test_description = new_test_description(test5) ! { dg-error "Rank mismatch in function result" }

contains

  logical function test1(arg)
    integer, intent(in) :: arg
    if (arg == 3) then
      test1 = .true.
    else
      test1 = .false.
    endif
  end function

  real function test2(arg)
    integer, intent(in) :: arg
    if (arg == 3) then
      test2 = 1.0
    else
      test2 = 0.0
    endif
  end function

  logical function test3()
    test3 = .false.
  end function

  logical function test4(arg)
    integer, intent(in) :: arg(:)
    if (sum (arg) == 3) then
      test4 = .true.
    else
      test4 = .false.
    endif
  end function

  function test5(arg) result(res)
    integer, intent(in) :: arg
    logical :: res(2)
    if (arg == 3) then
      res = .true.
    else
      res = .false.
    endif
  end function

end
