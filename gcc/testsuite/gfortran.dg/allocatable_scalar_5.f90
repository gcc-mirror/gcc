! { dg-do run }
! { dg-options "-Wall -pedantic" }
!
! PR fortran/41872; updated due to PR fortran/46484
!
!  More tests for allocatable scalars
!
program test
  implicit none
  integer, allocatable :: a
  integer :: b

  if (allocated (a)) STOP 1
  b = 7
  b = func(.true.)
  if (b /= 5332) STOP 2 
  b = 7
  b = func(.true.) + 1
  if (b /= 5333) STOP 3 
   
  call intout (a, .false.)
  if (allocated (a)) STOP 4
  call intout (a, .true.)
  if (.not.allocated (a)) STOP 5
  if (a /= 764) STOP 6
  call intout2 (a)
  if (allocated (a)) STOP 7

contains

  function func (alloc)
    integer, allocatable ::  func
    logical :: alloc
    if (allocated (func)) STOP 8
    if (alloc) then
      allocate(func)
      func = 5332
    end if
  end function func

  subroutine intout (dum, alloc)
    implicit none
    integer, allocatable,intent(out) :: dum
    logical :: alloc
    if (allocated (dum)) STOP 9
    if (alloc) then
      allocate (dum)
      dum = 764
    end if
  end subroutine intout

  subroutine intout2 (dum) ! { dg-warning "declared INTENT.OUT. but was not set" }
    integer, allocatable,intent(out) :: dum
  end subroutine intout2
end program test
