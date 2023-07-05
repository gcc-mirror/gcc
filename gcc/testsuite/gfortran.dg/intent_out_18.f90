! { dg-do run }
!
! PR fortran/92178
! Contributed by Mikael Morin

program p
  implicit none
  type t
    integer :: i
    integer, pointer :: pi
  end type t
  integer, target :: j
  type(t), allocatable :: ta
  j = 1
  ta = t(2, j)
  call assign(ta, id(ta%pi))
  if (ta%i /= 1) stop 1
  if (associated(ta%pi)) stop 2
contains
  subroutine assign(a, b)
    type(t), intent(out), allocatable :: a
    integer, intent(in) , value       :: b
    allocate(a)
    a%i = b
    a%pi => null()
  end subroutine assign
  function id(a)
    integer, pointer :: id, a
    id => a
  end function id
end program p
