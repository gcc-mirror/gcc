! { dg-do run }
! These used to segfault.
program main
  real, dimension(1,0) :: a, b, c
  integer, dimension(0) :: j
  a = 0
  c = 0
  b = cshift (a,1)
  b = cshift (a,j)
  b = eoshift (a,1)
  b = eoshift (a,1,boundary=c(1,:))
  b = eoshift (a, j, boundary=c(1,:))
end program main
