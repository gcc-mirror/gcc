! { dg-do compile }
! { dg-options "-O0 -fdump-tree-original" }
!
! PR fortran/56907
!
subroutine sub(xxx, yyy)
  use iso_c_binding
  implicit none
  integer, target, contiguous :: xxx(:)
  integer, target             :: yyy(:)
  type(c_ptr) :: ptr1, ptr2, ptr3, ptr4
  ptr1 = c_loc (xxx)
  ptr2 = c_loc (xxx(5:))
  ptr3 = c_loc (yyy)
  ptr4 = c_loc (yyy(5:))
end
! { dg-final { scan-tree-dump-not " _gfortran_internal_pack" "original" } }
! { dg-final { scan-tree-dump-times "parm.\[0-9\]+.data = \\(void .\\) &\\(.xxx.\[0-9\]+\\)\\\[0\\\];" 1 "original" } }
! { dg-final { scan-tree-dump-times "parm.\[0-9\]+.data = \\(void .\\) &\\(.xxx.\[0-9\]+\\)\\\[D.\[0-9\]+ \\* 4\\\];" 1 "original" } }
! { dg-final { scan-tree-dump-times "parm.\[0-9\]+.data = \\(void .\\) yyy.\[0-9\]+;" 1 "original" } }
! { dg-final { scan-tree-dump-times "parm.\[0-9\]+.data = \\(void .\\) yyy.\[0-9\]+ \\+ \\(sizetype\\) \\(D.\[0-9\]+ \\* 16\\);" 1 "original" } }

! { dg-final { scan-tree-dump-times "D.\[0-9\]+ = parm.\[0-9\]+.data;\[^;]+ptr\[1-4\] = D.\[0-9\]+;" 4 "original" } }
