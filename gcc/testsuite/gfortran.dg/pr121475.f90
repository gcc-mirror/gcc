! { dg-do run }
!
! PR fortran/121475 - Function result not finalized when passed to
! user-defined assignment.
!
! F2003 4.5.5.2 requires that function results be finalized after
! execution of the innermost executable construct.  When the function
! result is passed to a user-defined assignment, the finalization was
! being skipped because of early return for types with defined_assign_comp.

! Test case put together by Christopher Albert.

module m
  implicit none
  integer :: final_count = 0

  type :: base
  contains
    procedure :: assign
    generic :: assignment(=) => assign
  end type

  type, extends(base) :: derived
    integer :: val
  contains
    final :: finalize
  end type

  interface derived
    module procedure constructor
  end interface

contains

  subroutine finalize(self)
    type(derived), intent(inout) :: self
    final_count = final_count + 1
  end subroutine

  function constructor() result(self)
    type(derived) :: self
    self%val = 0
  end function

  subroutine assign(to, from)
    class(base), intent(out) :: to
    class(base), intent(in) :: from
    select type (to)
    type is (derived)
      select type (from)
      type is (derived)
        to%val = from%val
      end select
    end select
  end subroutine

end module

program test
  use m
  implicit none

  block
    type(derived) :: obj

    final_count = 0
    obj = derived()

    ! Function result and intermediate temporaries finalized
    if (final_count /= 2) stop 1
  end block

  ! obj goes out of scope, finalized again
  if (final_count /= 3) stop 2
end program
