! { dg-do compile }
! { dg-compile-aux-modules "whole_file_30.f90" }
! Test the fix for the problem described in PR46818.
!
! Contributed by Martien Hulsen  <m.a.hulsen@tue.nl>
! and reduced by Tobias Burnus  <burnus@gcc.gnu.org>
!
! ========== t.f90 ===========================
module convecreac_m
  use system_defs_m
  type(sysvector_t), pointer :: solution
end module convecreac_m

program t
  use convecreac_m
  implicit none
  type(sysvector_t), target :: sol
  solution => sol
end program t
! { dg-final { cleanup-modules "system_defs_m" } }
