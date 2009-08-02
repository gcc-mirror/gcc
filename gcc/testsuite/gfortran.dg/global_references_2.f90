! { dg-do compile }
! { dg-options "-std=legacy" }
!
! This program tests the patch for PR25964. This is a
! regression that would not allow a common block and a statement
! to share the same name.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
  common /foo/ a, b, c
  foo (x) = x + 1.0
  print *, foo (0.0)
  end

