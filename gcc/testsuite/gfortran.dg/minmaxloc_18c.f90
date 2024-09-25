! { dg-do compile }
! { dg-additional-files "minmaxloc_18.f90" }
! { dg-additional-options "-O3 -fno-inline-intrinsics=maxloc -fdump-tree-original" }
! { dg-final { scan-tree-dump-not "gfortran_\[sm\]?minloc" "original" } }
! { dg-final { scan-tree-dump-times "gfortran_\[sm\]?maxloc" 30 "original" } }
!
! PR fortran/90608
! Check that -O3 enables inlining and -fno-inline-intrinsics selectively
! disables it.
include "minmaxloc_18.f90"
