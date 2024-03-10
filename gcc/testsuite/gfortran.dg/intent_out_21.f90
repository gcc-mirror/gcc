! { dg-do run }
!
! PR fortran/92178
! Check that in the case of a data reference depending on its own content
! passed as actual argument to an INTENT(OUT) dummy, no reference to the
! content happens after the deallocation.

program p
  implicit none
  type t
    integer :: i
  end type t
  type u
    class(t), allocatable :: ta(:)
  end type u
  type(u), allocatable :: c(:)
  c = [u([t(1), t(3)]), u([t(4), t(9)])]
  call bar (                          &
      allocated (c(c(1)%ta(1)%i)%ta), &
      c(c(1)%ta(1)%i)%ta,             &
      allocated (c(c(1)%ta(1)%i)%ta)  &
  )
  if (allocated(c(1)%ta)) stop 11
  if (.not. allocated(c(2)%ta)) stop 12
contains
  subroutine bar (alloc, x, alloc2)
    logical :: alloc, alloc2
    class(t), allocatable, intent(out) :: x(:)
    if (allocated (x)) stop 1
    if (.not. alloc)   stop 2
    if (.not. alloc2)  stop 3
  end subroutine bar
end
