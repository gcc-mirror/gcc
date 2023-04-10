! { dg-do compile { target powerpc*-*-* } }
! { dg-options "-O2 -mabi=ibmlongdouble" }

! Test to ensure that it reports an "Cannot simplify expression" error
! instead of throwing an ICE when the memory represent of the HOLLERITH
! string is not unique with ibm long double encoding.

program main
  integer, parameter :: k = 16
  real(kind = k):: b = 4h1234
end program main

! { dg-warning "Conversion from HOLLERITH" "warning" { target powerpc*-*-* } 10 }
! { dg-error "Cannot simplify expression" "error" { target powerpc*-*-* } 10 }
