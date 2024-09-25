! { dg-do compile }
! { dg-additional-files "minmaxloc_18.f90" }
! { dg-additional-options "-Os -fdump-tree-original" }
! { dg-final { scan-tree-dump-times "gfortran_\[sm\]?minloc" 30 "original" } }
! { dg-final { scan-tree-dump-times "gfortran_\[sm\]?maxloc" 30 "original" } }
!
! PR fortran/90608
! Check that all MINLOC and MAXLOC intrinsics use the implementation provided
! by the library when optimizing for size.
include "minmaxloc_18.f90"
