! { dg-do compile }
! { dg-options "-fdump-tree-original -fcoarray=single" }
!
! PR fortran/57906
! PR fortran/52052
!
type t
  integer, allocatable :: x(:)[:]
  class(*), allocatable :: z(:)[:]
  class(*), allocatable :: d[:]
end type t
type t2
  type(t) :: y
end type t2
type(t2) :: a, b
a = b
end

! { dg-final { scan-tree-dump "a.y.x.data = D.\[0-9\]+.y.x.data;" "original" } }
! { dg-final { scan-tree-dump "a.y.z._data.data = D.\[0-9\]+.y.z._data.data;" "original" } }
! { dg-final { scan-tree-dump "a.y.d._data.data = D.\[0-9\]+.y.d._data.data;" "original" } }
! { dg-final { cleanup-tree-dump "original" } }
