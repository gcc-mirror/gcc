! { dg-do compile }
! Tests the fix for PR34471 in which function KINDs that were
! USE associated would cause an error.  This checks a regression
! caused by an intermediate version of the patch.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
real (bad_kind(0d0)) function foo () ! { dg-error "must be an intrinsic or" }
  foo = real (kind (foo))
end function
