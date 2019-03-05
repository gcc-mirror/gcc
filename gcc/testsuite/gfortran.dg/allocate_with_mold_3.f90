! { dg-do  run }
! PR fortran/89174 - this used to segfault on execution.
! Test case by Neil Carlson.
module mod
  type :: array_data
    class(*), allocatable :: mold
  contains
    procedure :: push
  end type
contains
  subroutine push(this, value)
    class(array_data), intent(inout) :: this
    class(*), intent(in) :: value
    allocate(this%mold, mold=value) ! <== SEGFAULTS HERE
  end subroutine
end module

use mod
type(array_data) :: foo
call foo%push(42)
end
