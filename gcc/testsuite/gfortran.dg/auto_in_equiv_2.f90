! { dg-do run }
! { dg-options "-fdec-static -frecursive -fno-automatic" }

! Contributed by Mark Eggleston <mark.eggleston@codethink.com>
!
! Check that -fno-automatic does not break recursion. The recursive
! function is not marked with the resursive key word consequently
! local variables can be made static when -fno-automatic is used. The
! recursive function contains an equivalence that has a variable with
! the automatic attribute and one without.
!
include "auto_in_equiv_1.f90"

! { dg-warning "Flag '-fno-automatic' overwrites '-frecursive'" "warning" { target *-*-* } 0 } 
