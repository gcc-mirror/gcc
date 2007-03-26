! { dg-do run }
! Tests the fix for PR27269 and PR27xxx.
! The former caused a segfault in trying to process
! module b, with an unused equivalence in a. The latter
! produced an assembler error due to multiple declarations
! for a module equivalence, when one of the variables was
! initialized, as M in module a.
!
module a
  integer, parameter :: dp = selected_real_kind (10)
  real(dp) :: reM, M = 1.77d0
  equivalence (M, reM)
end module a

module b
  use a, only : dp
end module b

  use a
  use b
  if (reM .ne. 1.77d0) call abort ()
  reM = 0.57d1
  if (M .ne. 0.57d1) call abort ()
end
! { dg-final { cleanup-modules "a b" } }
