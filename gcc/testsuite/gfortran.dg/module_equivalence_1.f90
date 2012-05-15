! { dg-do run }
! This tests the fix for PR17917, where equivalences were not being
! written to and read back from modules.
!
! Contributed by Paul Thomas  pault@gcc.gnu.org
!
module test_equiv !Bug 17917
  common /my_common/ d
  real    a(2),b(4),c(4), d(8)
  equivalence (a(1),b(2)), (c(1),d(5))
end module test_equiv

subroutine foo ()
  use test_equiv, z=>b
  if (any (d(5:8)/=z)) call abort ()
end subroutine foo

program module_equiv
  use test_equiv
  b = 99.0_4
  a = 999.0_4
  c = (/99.0_4, 999.0_4, 999.0_4, 99.0_4/)
  call foo ()
end program module_equiv
