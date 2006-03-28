! Test of fix for PR18878
!
! Based on example in PR by Steve Kargl
!
module a
  integer, parameter :: b = kind(1.d0)
  real(b)            :: z
end module a
program d
  use a, only : e => b, f => b, u => z, v => z
  real(e) x
  real(f) y
  x = 1.e0_e
  y = 1.e0_f
  u = 99.0
  if (kind(x).ne.kind(y)) call abort ()
  if (v.ne.u) call abort ()
end program d

! { dg-final { cleanup-modules "a" } }
