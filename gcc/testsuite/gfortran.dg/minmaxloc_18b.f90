! { dg-do compile }
! { dg-additional-files "minmaxloc_18.f90" }
! { dg-additional-options "-O2 -fno-inline-intrinsics=minloc -fdump-tree-original" }
! { dg-final { scan-tree-dump-times "gfortran_\[sm\]?minloc" 30 "original" } }
! { dg-final { scan-tree-dump-not "gfortran_\[sm\]?maxloc" "original" } }
!
! PR fortran/90608
! Check that -O2 enables inlining and -fno-inline-intrinsics selectively
! disables it.
include "minmaxloc_18.f90"
