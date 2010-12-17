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

  if (allocated (a)) call abort ()
  b = 7
  b = func(.true.)
  if (b /= 5332) call abort () 
  b = 7
  b = func(.true.) + 1
  if (b /= 5333) call abort () 
   
  call intout (a, .false.)
  if (allocated (a)) call abort ()
  call intout (a, .true.)
  if (.not.allocated (a)) call abort ()
  if (a /= 764) call abort ()
  call intout2 (a)
  if (allocated (a)) call abort ()

contains

  function func (alloc)
    integer, allocatable ::  func
    logical :: alloc
    if (allocated (func)) call abort ()
    if (alloc) then
      allocate(func)
      func = 5332
    end if
  end function func

  subroutine intout (dum, alloc)
    implicit none
    integer, allocatable,intent(out) :: dum
    logical :: alloc
    if (allocated (dum)) call abort()
    if (alloc) then
      allocate (dum)
      dum = 764
    end if
  end subroutine intout

  subroutine intout2 (dum) ! { dg-warning "declared INTENT.OUT. but was not set" }
    integer, allocatable,intent(out) :: dum
  end subroutine intout2
end program test
