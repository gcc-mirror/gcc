! { dg-do run }
!
! PR fortran/110618
! Check that if a data reference is passed as actual argument whose dummy
! has INTENT(OUT) attribute, any other argument depending on the
! same data reference is evaluated before the data reference deallocation.

program p
  implicit none
  type t
    integer :: i
  end type t
  type u
    class(t), allocatable :: ta(:)
  end type u
  type(u), allocatable :: c(:)
  class(t), allocatable :: d(:)
  allocate(c, source = [u([t(1), t(3)]), u([t(4), t(9)])])
  allocate(d, source = [t(1), t(5)])
  call bar (                   &
      allocated(c(d(1)%i)%ta), &
      d,                       &
      c(d(1)%i)%ta,            &
      allocated (c(d(1)%i)%ta) &
  )
  if (allocated (c(1)%ta)) stop 11
  if (.not. allocated (c(2)%ta)) stop 11
contains
  subroutine bar (alloc, x, y, alloc2)
    logical :: alloc, alloc2
    class(t), allocatable, intent(out) :: x(:)
    class(t), allocatable, intent(out) :: y(:)
    if (allocated (x)) stop 1
    if (.not. alloc)   stop 2
    if (.not. alloc2)  stop 3
  end subroutine bar
end
