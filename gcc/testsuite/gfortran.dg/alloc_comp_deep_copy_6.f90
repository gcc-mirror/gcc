! { dg-do run }
! { dg-additional-options "-Wa,--noexecstack" { target gas } }
! { dg-additional-options "-Wl,-z,noexecstack" { target gld } }
!
! PR fortran/121628
! Test deep copy of recursive allocatable components with both data arrays
! and recursive children. This is a comprehensive test combining:
! 1. Allocatable data arrays (values)
! 2. Recursive allocatable arrays (children)
! 3. Multi-level tree structure
! 4. Complete data integrity verification after deep copy
! 5. No trampolines (noexecstack flags)
!
! Contributed by Christopher Albert  <albert@tugraz.at>
!
program alloc_comp_deep_copy_6
  use, intrinsic :: iso_fortran_env, only: dp => real64
  implicit none

  type :: nested_t
    real(dp), allocatable :: values(:)
    type(nested_t), allocatable :: children(:)
  end type nested_t

  type(nested_t) :: a, b

  ! Build nested structure with both values and children
  allocate (b%values(3))
  b%values = [1.0_dp, 2.0_dp, 3.0_dp]

  allocate (b%children(2))
  allocate (b%children(1)%values(2))
  b%children(1)%values = [4.0_dp, 5.0_dp]

  allocate (b%children(2)%values(1))
  b%children(2)%values = [6.0_dp]

  ! Deeper nesting
  allocate (b%children(1)%children(1))
  allocate (b%children(1)%children(1)%values(2))
  b%children(1)%children(1)%values = [7.0_dp, 8.0_dp]

  ! Deep copy
  a = b

  ! Verify allocation status
  if (.not. allocated(a%values)) stop 1
  if (.not. allocated(a%children)) stop 2
  if (.not. allocated(a%children(1)%values)) stop 3
  if (.not. allocated(a%children(2)%values)) stop 4
  if (.not. allocated(a%children(1)%children)) stop 5
  if (.not. allocated(a%children(1)%children(1)%values)) stop 6

  ! Verify data integrity
  if (any(a%values /= [1.0_dp, 2.0_dp, 3.0_dp])) stop 7
  if (any(a%children(1)%values /= [4.0_dp, 5.0_dp])) stop 8
  if (any(a%children(2)%values /= [6.0_dp])) stop 9
  if (any(a%children(1)%children(1)%values /= [7.0_dp, 8.0_dp])) stop 10

  ! Verify deep copy: modify a and ensure b is unchanged
  a%values(1) = -1.0_dp
  a%children(1)%values(1) = -2.0_dp
  a%children(2)%values(1) = -3.0_dp
  a%children(1)%children(1)%values(1) = -4.0_dp

  if (any(b%values /= [1.0_dp, 2.0_dp, 3.0_dp])) stop 11
  if (any(b%children(1)%values /= [4.0_dp, 5.0_dp])) stop 12
  if (any(b%children(2)%values /= [6.0_dp])) stop 13
  if (any(b%children(1)%children(1)%values /= [7.0_dp, 8.0_dp])) stop 14

  if (any(a%values /= [-1.0_dp, 2.0_dp, 3.0_dp])) stop 15
  if (any(a%children(1)%values /= [-2.0_dp, 5.0_dp])) stop 16
  if (any(a%children(2)%values /= [-3.0_dp])) stop 17
  if (any(a%children(1)%children(1)%values /= [-4.0_dp, 8.0_dp])) stop 18
end program alloc_comp_deep_copy_6
