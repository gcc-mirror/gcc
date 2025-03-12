! { dg-do compile }
! { dg-compile-aux-modules "binding_label_tests_26a.f90" }
!
! PR 58182: [4.9 Regression] ICE with global binding name used as a FUNCTION
!
! Contributed by Andrew Bensons <abensonca@gmail.com>

module f
  use fg
end module

! { dg-final { cleanup-modules "fg f" } }
