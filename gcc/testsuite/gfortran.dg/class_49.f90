! { dg-do compile }
!
! PR fortran/52029
!

elemental subroutine foo()
  type t
  end type t
  class(t), allocatable :: x
  if (allocated(x)) i = 5
end
