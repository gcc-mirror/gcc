! { dg-do  run }
! { dg-options "-ffrontend-optimize -fdump-tree-original" }
! PR 66041 - this used to ICE with an incomplete fix for the PR.
program main
  implicit none
  real, dimension(1,-2:0) :: a1
  real, dimension(3,2) :: b1
  real, dimension(2) :: c1

  data a1 /17., -23., 29./
  data b1 / 2.,  -3.,  5.,  -7., 11., -13./

  c1 = matmul(a1(1,:), b1)
  if (any (c1-[248., -749.] /= 0.)) STOP 1
end program main

! { dg-final { scan-tree-dump-times "_gfortran_matmul" 0 "original" } }
