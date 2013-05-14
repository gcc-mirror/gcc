! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/56079
!
use iso_c_binding
implicit none
type t
  type(c_ptr) :: ptr = c_null_ptr
end type t

type(t), parameter :: para = t()
integer(c_intptr_t) :: intg
intg = transfer (para, intg)
intg = transfer (para%ptr, intg)
end

! { dg-final { scan-tree-dump-times "intg = 0;" 2 "original" } }
! { dg-final { cleanup-tree-dump "original" } }

