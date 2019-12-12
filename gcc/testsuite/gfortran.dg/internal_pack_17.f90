! { dg-do compile }
! { dg-additional-options "-O0 -fdump-tree-original" }
! PR 59345 - pack/unpack was not needed here.
! Original test case by Joost VandeVondele 
SUBROUTINE S1(A)
 INTERFACE
   FUNCTION CONTIGUOUS_F1() RESULT(res)
    INTEGER :: res(5)
   END FUNCTION
 END INTERFACE
 CALL S2(CONTIGUOUS_F1())
END SUBROUTINE

SUBROUTINE S3(A)
 INTERFACE
   FUNCTION CONTIGOUOS_F2() RESULT(res)
    INTEGER, ALLOCATABLE :: res(:)
   END FUNCTION
 END INTERFACE
 PROCEDURE(CONTIGOUOS_F2), POINTER :: A
 CALL S2(A())
END SUBROUTINE
! { dg-final { scan-tree-dump-not "_gfortran_internal_pack" "original" } }
! { dg-final { scan-tree-dump-not "_gfortran_internal_unpack" "original" } }
