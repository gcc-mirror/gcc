! { dg-do compile }
! { dg-options "" }
!
! Make sure we still see an error for missing exponents without -fdec.
!

implicit none

real, parameter :: r1 = 8e ! { dg-error "Missing exponent" }
real, volatile :: r2
r2 = 8e ! { dg-error "Missing exponent" }

end
