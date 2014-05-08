! { dg-do compile }
! { dg-options "-fdump-tree-original -fcoarray=lib" }
!
! CO_SUM/CO_MIN/CO_MAX
!
program test
  implicit none
  intrinsic co_max
  integer :: stat1, stat2, stat3
  character(len=6) :: errmesg1
  character(len=7) :: errmesg2
  character(len=8) :: errmesg3
  real :: val1
  complex, allocatable :: val2(:)
  character(len=99) :: val3
  integer :: res

  call co_max(val1, stat=stat1, errmsg=errmesg1)
  call co_sum(val2, result_image=4, stat=stat2, errmsg=errmesg2)
  call co_min(val3, result_image=res,stat=stat3, errmsg=errmesg3)
end program test

! { dg-final { scan-tree-dump-times "_gfortran_caf_co_max \\(&desc.., 0B, 0, &stat1, errmesg1, 0, 6\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_co_sum \\(&val2, 0B, 4, &stat2, errmesg2, 7\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_co_min \\(&desc.., 0B, res, &stat3, errmesg3, 99, 8\\);" 1 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
