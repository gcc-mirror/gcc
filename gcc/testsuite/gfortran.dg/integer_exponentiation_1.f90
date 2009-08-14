! { dg-do run }
! { dg-options "-pedantic -mieee" { target alpha*-*-* } }
! PR 30981 - this used to go into an endless loop during execution.
program test
  a = 3.0
  b = a**(-2147483647_4-1_4) ! { dg-warning "Integer outside symmetric range" }
end program test
