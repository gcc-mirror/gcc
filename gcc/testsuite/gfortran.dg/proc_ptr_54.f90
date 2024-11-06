! { dg-do compile }
!
! Test the fix for pr117434, in which the F2008 addition of being permitted to
! pass an external, internal or module procedure to a dummy procedure pointer
! gave the error "Expected a procedure pointer for argument ‘<arg_name>’ at (1).
!
! This testcase checks for correct results.
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

module test_mod

contains

  logical function mod_test(arg)
    integer, intent(in) :: arg
    if (arg == 1) then
      mod_test = .true.
    else
      mod_test = .false.
    endif
  end function

end

logical function ext_test(arg)
  integer, intent(in) :: arg
  if (arg == 2) then
    ext_test = .true.
  else
    ext_test = .false.
  endif
end function

  use julienne_test_description_m
  use test_mod
  implicit none
  type(test_description_t) test_description

  interface
    logical function ext_test(arg)
      integer, intent(in) :: arg
    end function
  end interface

  test_description = new_test_description(test)
  if (test_description%test_function_(1) &
      .or. test_description%test_function_(2) &
      .or. .not.test_description%test_function_(3)) stop 1

  test_description = new_test_description(mod_test)
  if (test_description%test_function_(2) &
      .or. test_description%test_function_(3) &
      .or. .not.test_description%test_function_(1)) stop 2

  test_description = new_test_description(ext_test)
  if (test_description%test_function_(1) &
      .or. test_description%test_function_(3) &
      .or. .not.test_description%test_function_(2)) stop 3

contains

  logical function test(arg)
    integer, intent(in) :: arg
    if (arg == 3) then
      test = .true.
    else
      test = .false.
    endif
  end function

end
