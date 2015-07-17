! { dg-do compile }
! { dg-options "-fdump-tree-original -fcoarray=lib" }
!
! CO_REDUCE
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

  call co_reduce(val1, operator=fr, result_image=num_images(), stat=stat1, errmsg=errmesg1)
  call co_reduce(val2, operator=gz, result_image=4, stat=stat2, errmsg=errmesg2)
  call co_reduce(val3, operator=hc, result_image=res,stat=stat3, errmsg=errmesg3)
contains
  pure real function fr(x,y)
    real, value :: x, y
    fr = x * y
  end function fr
  pure complex function gz(x,y)
    complex, intent(in):: x, y
    gz = x *y
  end function gz
  pure character(len=99) function hc(x,y)
    character(len=99), intent(in):: x, y
    hc = x(1:50) // y(1:49)
  end function hc
end program test

! { dg-final { scan-tree-dump-times "_gfortran_caf_co_reduce \\(&desc.., fr, 4, _gfortran_caf_num_images \\(0, -1\\), &stat1, errmesg1, 0, 6\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_co_reduce \\(&val2, gz, 0, 4, &stat2, errmesg2, 0, 7\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_co_reduce \\(&desc.., hc, 1, res, &stat3, errmesg3, 99, 8\\);" 1 "original" } }
