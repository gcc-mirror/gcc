! { dg-do run }
! PR 30981 - this used to go into an endless loop during execution.
program test
  a = 3.0
  b = a**(-2147483647-1) ! { dg-warning "Integer outside symmetric range" }
end program test
