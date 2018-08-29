! { dg-do run }
!
! PR fortran/37336
!
! Test for finalization of nonallocatable variables
!
module m
  implicit none
  type t
    integer :: i
  contains
    final :: finit
  end type t
  integer, save :: called_final = -1
contains
  impure elemental subroutine finit(x)
    type(t), intent(in) :: x
    if (called_final == -1) STOP 1
    called_final = called_final + 1 
    if (called_final /= x%i) STOP 2
  end subroutine finit
end module m

  use m
  implicit none
  type(t) :: x2, y2(2)
  block
    type(t) :: xx, yy(2)
    type(t), save :: x3, y3(2)
    yy%i = [1, 2]
    xx%i = 3
    y3%i = [-4, -5]
    x3%i = -6
    called_final = 0
  end block
  if (called_final /= 3) STOP 1
  called_final = -1
  y2%i = [-7, -8]
  x2%i = -9
end
