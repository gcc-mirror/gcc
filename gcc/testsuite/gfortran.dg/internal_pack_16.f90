! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
! PR 59345 - pack/unpack was not needed here.
SUBROUTINE S1(A)
 REAL :: A(3)
 CALL S2(-A)
END SUBROUTINE
! { dg-final { scan-tree-dump-not "_gfortran_internal_pack" "original" } }
! { dg-final { scan-tree-dump-not "_gfortran_internal_unpack" "original" } }
