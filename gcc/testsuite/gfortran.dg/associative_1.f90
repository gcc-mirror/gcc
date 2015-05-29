! { dg-do compile }
! { dg-options "-O1 -fno-signed-zeros -fno-trapping-math -fdump-tree-optimized" }
! Fortran defaults to associative by default,
! with -fno-signed-zeros -fno-trapping-math this should optimize away all additions
SUBROUTINE S1(a)
 REAL :: a
 a=1+a-1
END SUBROUTINE S1
! { dg-final { scan-tree-dump-times " \\\+ " 0 "optimized" } }
