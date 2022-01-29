! { dg-do compile }
! { dg-additional-options "-fdump-tree-original -fcoarray=lib" }
!
! PR 103970
! Test case inspired by code submitted by Damian Rousson

program main

  implicit none

  type foo_t
    integer i
    integer, allocatable :: j
  end type

  type(foo_t) foo
  integer, parameter :: source_image = 1

  if (this_image() == source_image)  then
    foo = foo_t(2,3)
  else
    allocate(foo%j)
  end if
  call co_broadcast(foo, source_image)

  if ((foo%i /= 2) .or. (foo%j /= 3))  error stop 1
  sync all

end program

! Wrong code generation produced too many temp descriptors
! leading to stacked descriptors handed to the co_broadcast.
! This lead to access to non exsitant memory in opencoarrays.
! In single image mode just checking for reduced number of
! descriptors is possible, i.e., execute always works.
! { dg-final { scan-tree-dump-times "desc\\.\[0-9\]+" 12 "original" } }

