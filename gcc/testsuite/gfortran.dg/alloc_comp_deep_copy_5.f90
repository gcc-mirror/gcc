! { dg-do run }
! { dg-additional-options "-Wa,--noexecstack" { target { ! *-*-darwin* } } }
! { dg-additional-options "-Wl,-z,noexecstack" { target { ! *-*-darwin* } } }
!
! PR fortran/121628
! Test deep copy of recursive allocatable array components with multi-level
! nesting and repeated circular assignments. This test ensures:
! 1. Deep copy works correctly for grandchildren (multi-level recursion)
! 2. Repeated circular assignments don't cause memory corruption/double-free
! 3. No trampolines are generated (verified by noexecstack flags)
!
! Contributed by Christopher Albert  <albert@tugraz.at>
!            and Harald Anlauf  <anlauf@gcc.gnu.org>
!
program alloc_comp_deep_copy_5
  implicit none

  type :: nested_t
     character(len=10)             :: name
     type(nested_t),   allocatable :: children(:)
  end type nested_t

  type(nested_t) :: a, b

  ! Build a tree with grandchildren
  b%name = "root"
  allocate (b%children(2))
  b%children(1)%name = "child1"
  b%children(2)%name = "child2"
  allocate (b%children(1)%children(1))
  b%children(1)%children(1)%name = "grandchild"

  ! Test 1: Initial assignment
  a = b
  if (.not. allocated(a%children)) stop 1
  if (.not. allocated(a%children(1)%children)) stop 2
  if (a%children(1)%children(1)%name /= "grandchild") stop 3

  ! Verify deep copy by modifying a
  a%children(1)%children(1)%name = "modified"
  if (b%children(1)%children(1)%name /= "grandchild") stop 4
  if (a%children(1)%children(1)%name /= "modified") stop 5

  ! Test 2: Circular assignment b=a (should not corrupt memory)
  b = a
  if (.not. allocated(a%children)) stop 6
  if (.not. allocated(a%children(1)%children)) stop 7
  if (.not. allocated(b%children)) stop 8
  if (.not. allocated(b%children(1)%children)) stop 9

  ! Test 3: Circular assignment a=b (stress test)
  a = b
  if (.not. allocated(a%children)) stop 10
  if (.not. allocated(a%children(1)%children)) stop 11

  ! Test 4: Another circular assignment (triggered double-free in buggy code)
  b = a
  if (.not. allocated(b%children)) stop 12
  if (.not. allocated(b%children(1)%children)) stop 13

  ! Verify final state
  if (b%children(1)%children(1)%name /= "modified") stop 14
end program alloc_comp_deep_copy_5
