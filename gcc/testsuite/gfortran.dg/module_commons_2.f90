! { dg-do compile }
! Tests the fix for PR35474, in which the PRIVATE statement would
! cause the error Internal Error at (1): free_pi_tree(): Unresolved fixup
! This arose because the symbol for 'i' emanating from the COMMON was
! not being fixed-up as the EQUIVALENCE was built.
!
! Contributed by FX Coudert <fxcoudert@gcc.gnu.org>
!
module h5global
  integer i
  integer j
  common /c/ i
  equivalence (i, j)
  private
end module h5global

program bug
  use h5global
end

! { dg-final { cleanup-modules "h5global" } }
