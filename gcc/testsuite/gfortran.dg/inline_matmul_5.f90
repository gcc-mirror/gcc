! { dg-do  run }
! { dg-options "-ffrontend-optimize" }
program main

  real, dimension(2,2) :: a,b,c

  data a /2., 4., 8., 16. /
  data b /3., 9., 27., 81./

  c = matmul(a,b)
  a = matmul(a,b)
  if (any(a /= c)) call abort
end program main
