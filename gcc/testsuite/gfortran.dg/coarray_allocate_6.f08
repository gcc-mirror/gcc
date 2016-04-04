! { dg-do run }
! { dg-options "-fcoarray=single -fdump-tree-original" }

! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
! Test fix for pr65795.

implicit none

type t2
  integer, allocatable :: x
end type t2

type t3
  type(t2), allocatable :: caf[:]
end type t3

!type(t3), save, target :: c, d
type(t3), target :: c, d
integer :: stat

allocate(c%caf[*], stat=stat)
end

! Besides checking that the executable does not crash anymore, check
! that the cause has been remove.
! { dg-final { scan-tree-dump-not "c.caf.x = 0B" "original" } }

