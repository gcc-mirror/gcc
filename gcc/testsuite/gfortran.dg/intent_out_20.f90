! { dg-do run }
!
! PR fortran/92178
! Check that if a data reference passed is as actual argument whose dummy
! has INTENT(OUT) attribute, any other argument depending on the
! same data reference is evaluated before the data reference deallocation.

program p
  implicit none
  type t
    integer :: i
  end type t
  type u
    class(t), allocatable :: ta
  end type u
  type(u),  allocatable :: c(:)
  allocate(c, source = [u(t(1)), u(t(4))])
  call bar (                         &
      allocated (c(c(1)%ta%i)%ta), &
      c(c(1)%ta%i)%ta,            &
      allocated (c(c(1)%ta%i)%ta) &
  )
  if (allocated (c(1)%ta)) stop 11
  if (.not. allocated (c(2)%ta)) stop 12
contains
  subroutine bar (alloc, x, alloc2)
    logical :: alloc, alloc2
    class(t), allocatable, intent(out) :: x(..)
    if (allocated (x)) stop 1
    if (.not. alloc)   stop 2
    if (.not. alloc2)  stop 3
  end subroutine bar
end
