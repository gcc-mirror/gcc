! { dg-do run }
program main
  real, dimension(3,2) :: a
  real, dimension(3) :: bp
  real, dimension(3) :: res1
  real, dimension(:), allocatable :: c3
  real, dimension(2) :: res2

  data a /-2., 3., -5., 7., -11., 13./
  data bp /-23., -31., -41./
  data res2 /158., -353./

  c3 = matmul(bp,a)
  if (size(c3,1) /= 2) STOP 1
  if (any(c3 /= res2)) STOP 2

end program main
