! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/58436
!
! The following was ICEing and lacking _final=0
!
class(*), allocatable :: var
end

! { dg-final { scan-tree-dump "static struct __vtype__STAR __vtab__STAR = {._hash=0, ._size=., ._extends=0B, ._def_init=0B, ._copy=0B, ._final=0B, ._deallocate=0B};" "original" } }
