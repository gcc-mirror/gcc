! { dg-do run }
!
! Test the fix for PR57710.
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
module m
  type t
  end type t
  type t2
    integer :: ii
    class(t), allocatable :: x
  end type t2
contains
  subroutine fini(x)
     type(t) :: x
  end subroutine fini
end module m

use m
block
  type(t) :: z
  type(t2) :: y
  y%ii = 123
  if (.not. same_type_as(y%x, z)) call abort ()
end block
end
