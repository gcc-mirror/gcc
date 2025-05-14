! { dg-do run }
! { dg-additional-options "-Wexternal-argument-mismatch" }
! PR 119074 - the code is legal, but it makes sense to warn anyway.

program main
  external ex1,ex2
  call foo(ex1,1)
  call foo(ex2,2)
end program main

subroutine ex1(n)
  integer :: n
  if (n /= 1) error stop
end subroutine ex1

subroutine ex2(n,m)
  integer :: n,m
  if (n /= 2) error stop
  if (m /= 3) error stop
end subroutine ex2

subroutine foo(a,n)
  external a
  if (n == 1) call a(1)   ! { dg-warning "Different argument lists" }
  if (n == 2) call a(2,3) ! { dg-warning "Different argument lists" }
end subroutine foo
