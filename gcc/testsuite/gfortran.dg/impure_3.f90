! { dg-do compile }
! { dg-options "-std=f95" }

! PR fortran/45197
! Check that IMPURE gets rejected without F2008.

! Contributed by Daniel Kraft, d@domob.eu.

IMPURE SUBROUTINE foobar () ! { dg-error "Fortran 2008" }

IMPURE ELEMENTAL FUNCTION xyz () ! { dg-error "Fortran 2008" }
