! { dg-do compile }
! { dg-options "-fallow-argument-mismatch" }
program main
  real :: a(10), b(10,10)
! This should be caugt
  call foo(1.0) ! { dg-warning "Rank mismatch" }
  call foo(b)   ! { dg-warning "Rank mismatch" }
! This is OK
  call bar(a)
  call bar(b)

end program main
