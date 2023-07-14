! { dg-do run }
!
! PR fortran/92178
! Check that if a data reference passed is as actual argument whose dummy
! has INTENT(OUT) attribute, any other argument depending on the
! same data reference is evaluated before the data reference deallocation.

program p
  implicit none
  class(*),  allocatable :: c
  c = 3
  call bar (allocated(c), c, allocated (c))
  if (allocated (c)) stop 14
contains
  subroutine bar (alloc, x, alloc2)
    logical :: alloc, alloc2
    class(*), allocatable, intent(out) :: x(..)
    if (allocated (x)) stop 5
    if (.not. alloc)   stop 6
    if (.not. alloc2)  stop 16
  end subroutine bar
end
