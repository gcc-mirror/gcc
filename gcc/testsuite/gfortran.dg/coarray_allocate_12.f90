! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/105184
! Based on testcases by Gerhard Steinmetz

program p
  real, allocatable :: x[:,:]
  integer :: n = 2
  allocate (x[  n, *])
  allocate (x[1:n, *])
  allocate (x[n:n, *])
  allocate (x[n, 5:*])
  allocate (x[ :n,   *]) ! { dg-error "Bad coarray specification" }
  allocate (x[::n,   *]) ! { dg-error "Bad coarray specification" }
  allocate (x[ :1:1, *]) ! { dg-error "Bad coarray specification" }
  allocate (x[1:n:n, *]) ! { dg-error "Bad coarray specification" }
  allocate (x[1,   : *]) ! { dg-error "Missing lower bound" }
end
