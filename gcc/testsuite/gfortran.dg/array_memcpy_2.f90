! This checks that the "z = y" assignment is not considered copyable, as the
! array is of a derived type containing allocatable components.  Hence, we
! we should expand the scalarized loop, which contains *two* memcpy calls
! for the assignment itself, plus one for initialization.
! { dg-do compile }
! { dg-options "-O2 -fdump-tree-original" }
!
! PR 121628
!
  type :: a
    integer, allocatable :: i(:)
  end type a

  type :: b
    type (a), allocatable :: at(:)
  end type b

  type(b) :: y(2), z(2)
  integer :: j

  do j = 1, 2
    allocate(y(j)%at(1))
    allocate(y(j)%at(1)%i(1))
    y(j)%at(1)%i(1) = j
  end do

  z = y
end
! { dg-final { scan-tree-dump-times "__builtin_memcpy" 4 "original" } }
