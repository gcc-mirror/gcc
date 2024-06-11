! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/58652
!
! Contributed by Vladimir Fuka
!
  class(*),allocatable :: a
  class(*),allocatable :: c
  call move_alloc(a,c)
end

! { dg-final { scan-tree-dump "c._vptr = a._vptr;" "original" } }
