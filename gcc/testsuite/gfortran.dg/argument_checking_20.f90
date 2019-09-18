! { dg-do compile }
program main
  real :: a(10), b(10,10)
! This should be caugt
  call foo(1.0) ! { dg-error "Rank mismatch" }
  call foo(b)   ! { dg-error "Rank mismatch" }
! This is OK
  call bar(a)
  call bar(b)

end program main
