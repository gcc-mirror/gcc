! { dg-do compile }
! { dg-options "-fcoarray=lib -fdump-tree-original" }
!
! Allocate/deallocate with libcaf.
!

 subroutine test()
 integer(4), allocatable :: xx[:], yy(:)[:]
 integer :: stat
 character(len=200) :: errmsg
 allocate(xx[*], stat=stat, errmsg=errmsg)
 allocate(yy(2)[*], stat=stat, errmsg=errmsg)
 deallocate(xx,yy,stat=stat, errmsg=errmsg)
 end

! { dg-final { scan-tree-dump-times "_gfortran_caf_register .4, 1, &xx.token, &stat.., &errmsg, 200.;" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_register .8, 1, &yy.token, &stat.., &errmsg, 200.;" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_deregister .&xx.token, &stat.., &errmsg, 200.;" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_deregister .&yy.token, &stat.., &errmsg, 200.;" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_deregister .&yy.token, 0B, 0B, 0.;" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_deregister .&xx.token, 0B, 0B, 0.;" 1 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
