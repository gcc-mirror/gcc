! { dg-do compile }
! { dg-options "-Wall -fdump-tree-original" }
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
program main
  implicit none

  type :: struct
    real, allocatable :: var(:)
  end type struct

  type(struct) :: single
  real, allocatable :: ref1(:), ref2(:), ref3(:), ref4(:)

  ref2 = [1,2,3,4,5]    ! Warnings here

  single%var = ref2     ! No warnings for components
  ref1 = single%var     ! Warnings here
  ref1 = [1,2,3,4,5]    ! Should not add to tree dump count

  allocate (ref3(5))
  ref3 = single%var     ! No warnings following allocation

  call set_ref4

  call test (ref1)
  call test (ref2)
  call test (ref3)
  call test (ref4)

contains
  subroutine test (arg)
    real, allocatable :: arg(:)
    if (size(arg) /= size(single%var)) stop 1
    if (lbound(arg, 1) /= 1) stop 2
    if (any (arg /= single%var)) stop 3
  end
  subroutine set_ref4
    ref4 = single%var   ! Warnings in contained scope
  end
end
! { df-final { scan-tree-dump-times "ubound = 0" 3 "original" } }
