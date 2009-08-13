! { dg-do compile }
! { dg-options "-Wsurprising" }
!
! PR 40995: [4.5 Regression] Spurious "Type specified for intrinsic function...ignored" message
!
! Contributed by Mat Cross <mathewc@nag.co.uk>

subroutine sub(n,x)
  intrinsic abs
  integer n, x(abs(n))
end

