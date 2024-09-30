! { dg-do compile }
! { dg-additional-files "minmaxloc_18.f90" }
! { dg-additional-options "-O0 -finline-intrinsics=maxloc -fdump-tree-original" }
! { dg-final { scan-tree-dump-times "gfortran_\[sm\]?minloc" 30 "original" } }
! { dg-final { scan-tree-dump-not "gfortran_\[sm\]?maxloc" "original" } }
!
! PR fortran/90608
! Check that -O0 disables inlining and -finline-intrinsics selectively
! enables it.
include "minmaxloc_18.f90"
