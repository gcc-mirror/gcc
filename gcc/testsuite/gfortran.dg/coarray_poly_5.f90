! { dg-do compile }
! { dg-options "-fcoarray=lib -fdump-tree-original" }

subroutine test(x)
type t
  real, allocatable :: x[:]
end type t

class(t) :: x
allocate(x%x[*])
end subroutine test

! { dg-final { scan-tree-dump-times "x->_data->x.data = _gfortran_caf_register \\(4, 1, &x->_data->x.token, 0B, 0B, 0\\);" 1 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
