! { dg-do compile }
! { dg-options "-fcoarray=lib -fdump-tree-original" }
!
! Allocate/deallocate with libcaf.
!

 subroutine test()
 type t
 end type t
 class(t), allocatable :: xx[:], yy(:)[:]
 integer :: stat
 character(len=200) :: errmsg
 allocate(xx[*], stat=stat, errmsg=errmsg)
 allocate(yy(2)[*], stat=stat, errmsg=errmsg)
 deallocate(xx,yy,stat=stat, errmsg=errmsg)
 end

! { dg-final { scan-tree-dump-times "_gfortran_caf_register \\(1, 1, &xx._data.token, \\(void \\*\\) &xx._data, &stat.., &errmsg, 200\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_register \\(1, 1, &yy._data.token, \\(void \\*\\) &yy._data, &stat.., &errmsg, 200\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_deregister .&xx._data.token, &stat.., &errmsg, 200.;" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_deregister .&yy._data.token, &stat.., &errmsg, 200.;" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_deregister .&yy._data.token, 0B, 0B, 0.;" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_deregister .&xx._data.token, 0B, 0B, 0.;" 1 "original" } }
