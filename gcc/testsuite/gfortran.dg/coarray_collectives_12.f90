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

  call co_broadcast(val1, source_image=num_images(), stat=stat1, errmsg=errmesg1)
  call co_broadcast(val2, source_image=4, stat=stat2, errmsg=errmesg2)
  call co_broadcast(val3, source_image=res,stat=stat3, errmsg=errmesg3)
end program test

! { dg-final { scan-tree-dump-times "_gfortran_caf_co_broadcast \\(&desc.., _gfortran_caf_num_images \\(0, -1\\), &stat1, errmesg1, 6\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_co_broadcast \\(&val2, 4, &stat2, errmesg2, 7\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_co_broadcast \\(&desc.., res, &stat3, errmesg3, 8\\);" 1 "original" } }
