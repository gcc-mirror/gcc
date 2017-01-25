! { dg-do compile }
! { dg-options "-Winteger-division" }
program main
  print *,10**(-3) ! { dg-warning "Negative exponent of integer has zero result" }
end program main
