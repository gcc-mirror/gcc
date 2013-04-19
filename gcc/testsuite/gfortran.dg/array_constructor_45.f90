! { dg-do run }
! PR PR 56872 - wrong front-end optimization with a
! single array constructor and another value.
program main
  real    :: s
  integer :: m
  integer :: k
  real :: res

  m = 2
  s = 1000.

  res = SUM([3.0,(s**(REAL(k-1)/REAL(m-1)),k=1,m),17.])
  if (abs(res - 1021.)>1e-4) call abort
end
