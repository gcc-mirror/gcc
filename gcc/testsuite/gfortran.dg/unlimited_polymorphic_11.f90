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

! { dg-final { scan-tree-dump "\\(struct __vtype__STAR \\*\\) c._vptr = \\(struct __vtype__STAR \\*\\) a._vptr;" "original" } }
