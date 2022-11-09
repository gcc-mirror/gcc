! { dg-do compile }
! { dg-options "-std=f95" }
! PR fortran/107559 - ICE in resolve_equivalence
! Contributed by G.Steinmetz

module m
  implicit none
  integer, protected :: a ! { dg-error "Fortran 2003: PROTECTED attribute" }
  integer :: b
  equivalence (a, b)      ! { dg-error "has no IMPLICIT type" }
end
