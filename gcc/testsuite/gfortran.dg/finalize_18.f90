! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/37336
!
module m
  type t
  contains
    final :: fini
  end type t
  type t2
    integer :: ii
    type(t), allocatable :: aa
    type(t), allocatable :: bb(:)
    class(t), allocatable :: cc
    class(t), allocatable :: dd(:)
  end type t2
  integer, save :: cnt = -1
contains
  subroutine fini(x)
     type(t) :: x
     if (cnt == -1) call abort ()
     cnt = cnt + 1
  end subroutine fini
end module m

use m
block
  type(t2) :: y
  y%ii = 123
end block
end

! { dg-final { scan-tree-dump-times "if \\(y.aa != 0B\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "if \\(y.cc._data != 0B\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "if \\(\\(struct t\\\[0:\\\] . restrict\\) y.bb.data != 0B\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "if \\(\\(struct t\\\[0:\\\] . restrict\\) y.dd._data.data != 0B\\)" 1 "original" } }

! { dg-final { scan-tree-dump-times "desc.\[0-9\]+.data = \\(void . restrict\\) y.aa;" 1 "original" } }
! { dg-final { scan-tree-dump-times "desc.\[0-9\]+.data = \\(void . restrict\\) y.cc._data;" 1 "original" } }

! { dg-final { scan-tree-dump-times "__final_m_T \\(&desc.\[0-9\]+, 0, 1\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "__final_m_T \\(&y.bb, 0, 1\\);" 1 "original" } }
! { dg-final { scan-tree-dump "y.cc._vptr->_final \\(&desc.\[0-9\]+, (\\(integer\\(kind=8\\)\\) )?y.cc._vptr->_size, 1\\);" "original" } }
! { dg-final { scan-tree-dump "y.dd._vptr->_final \\(&y.dd._data, (\\(integer\\(kind=8\\)\\) )?y.dd._vptr->_size, 1\\);" "original" } }

! { dg-final { cleanup-tree-dump "original" } }
