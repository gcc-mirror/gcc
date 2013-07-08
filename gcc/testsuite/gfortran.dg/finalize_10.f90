! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/37336
!
! Finalize nonallocatable INTENT(OUT)
!
module m
  type t
  end type t
  type t2
  contains
    final :: fini
  end type t2
contains
  elemental subroutine fini(var)
    type(t2), intent(inout) :: var
  end subroutine fini
end module m

subroutine foo(x,y,aa,bb)
  use m
  class(t), intent(out) :: x(:),y
  type(t2), intent(out) :: aa(:),bb
end subroutine foo

! Finalize CLASS + set default init
! { dg-final { scan-tree-dump-times "y->_vptr->_final \\(&desc.\[0-9\]+, y->_vptr->_size, 0\\);" 1 "original" } }
! { dg-final { scan-tree-dump       "__builtin_memcpy \\(\\(void .\\) y->_data, \\(void .\\) y->_vptr->_def_init, \\((unsigned long|unsigned int|character\\(kind=4\\))\\) y->_vptr->_size\\);" "original" } }
! { dg-final { scan-tree-dump-times "x->_vptr->_final \\(&x->_data, x->_vptr->_size, 0\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "x->_vptr->_copy \\(x->_vptr->_def_init, &x->_data\\);" 1 "original" } }

! FINALIZE TYPE:
! { dg-final { scan-tree-dump-times "parm.\[0-9\]+.data = \\(void \\*\\) &\\(\\*aa.\[0-9\]+\\)\\\[0\\\];" 1 "original" } }
! { dg-final { scan-tree-dump-times "__final_m_T2 \\(&parm.\[0-9\]+, 0, 0\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "desc.\[0-9\]+.data = \\(void \\* restrict\\) bb;" 1 "original" } }
! { dg-final { scan-tree-dump-times "__final_m_T2 \\(&desc.\[0-9\]+, 0, 0\\);" 1 "original" } }

! { dg-final { cleanup-tree-dump "original" } }
