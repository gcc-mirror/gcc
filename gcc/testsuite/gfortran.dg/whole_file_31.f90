! { dg-do compile }
! Test the fix for the problem described in PR46818.
! Note that the module file from whole_file_30.f90, 'system_defs_m',
! is needed for this test.
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
