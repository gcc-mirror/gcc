! { dg-do compile }
!
! Test the fix for PR119948, which used to fail as indicated below with,
! "Error: Bad allocate-object at (1) for a PURE procedure"
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module test_m
  implicit none

  type test_t
    integer, allocatable :: i
  end type

  interface
    pure module function construct_test(arg) result(test)
      implicit none
      type(test_t) :: test
      type(test_t), intent(in) :: arg
    end function
    pure module function construct_test_sub(arg) result(test)
      implicit none
      type(test_t) :: test
      type(test_t), intent(in) :: arg
    end function
  end interface

contains
  module procedure construct_test
    allocate(test%i, source = arg%i) ! Used to fail here
  end procedure
end module

submodule (test_m)test_s
contains
  module procedure construct_test_sub
    allocate(test%i, source = arg%i) ! This was OK.
  end procedure
end submodule

  use test_m
  type(test_t) :: res, dummy
  dummy%i = 42
  res = construct_test (dummy)
  if (res%i /= dummy%i) stop 1
  dummy%i = -42
  res = construct_test_sub (dummy)
  if (res%i /= dummy%i) stop 2
  deallocate (res%i, dummy%i)
end
