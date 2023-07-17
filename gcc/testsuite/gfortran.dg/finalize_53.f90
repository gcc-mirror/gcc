! { dg-do compile }
!
! Check that the data reference preliminary code is properly
! generated and accepted by the finalization handling code.

module m
  implicit none
  type t
    integer :: i
  contains
    final :: finalize_t
  end type t
  logical :: finalize_called = .false.
contains
  subroutine finalize_t(a)
    type(t) :: a
    finalize_called = .true.
  end subroutine finalize_t
end module m
program p
  use m
  type u
    type(t), allocatable :: ta
  end type u
  class(u), allocatable :: c(:)
  integer, allocatable :: a(:), b(:)
  a = [1, 2, 3]
  b = [3, 5, 1]
  allocate(c, source = [u(t(1)), u(t(9))])
  deallocate(c(count(a + b == 4))%ta)
  if (.not. allocated (c(1)%ta)) stop 11
  if (allocated (c(2)%ta)) stop 12
  if (.not. finalize_called) stop 13
end program p
