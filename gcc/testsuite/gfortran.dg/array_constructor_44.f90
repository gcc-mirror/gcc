! { dg-do run }
! { dg-options "-ffrontend-optimize" }
! PR 56872 - wrong front-end optimization with a single constructor.
! Original bug report by Rich Townsend.
  integer :: k
  real :: s
  integer :: m
  s = 2.0
  m = 4
  res = SUM([(s**(REAL(k-1)/REAL(m-1)),k=1,m)])
  if (abs(res - 5.84732246) > 1e-6) call abort
  end
