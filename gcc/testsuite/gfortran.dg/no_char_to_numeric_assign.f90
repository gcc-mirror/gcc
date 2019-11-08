! { dg-do compile }
! { dg-options "-fdec-char-conversions" }
!
! Test character variables can not be assigned to numeric and
! logical variables.
!
! Test case contributed by Mark Eggleston <mark.eggleston@codethink.com>
!
program test
  integer a
  real b
  complex c
  logical d
  character e

  e = "A"
  a = e ! { dg-error "Cannot convert" }
  b = e ! { dg-error "Cannot convert" }
  c = e ! { dg-error "Cannot convert" }
  d = e ! { dg-error "Cannot convert" }
end program
