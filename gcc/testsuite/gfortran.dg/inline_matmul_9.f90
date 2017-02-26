! { dg-do  run }
! { dg-options "-ffrontend-optimize -fdump-tree-optimized" }
! PR 66041 - this used to ICE with an incomplete fix for the PR.
program main
  implicit none
  type :: t
    real :: c
  end type t
  type(t), dimension(1,-2:0) :: a1
  real, dimension(3,2) :: b1
  real, dimension(2) :: c1
  real, dimension(1,2) :: c2

  data a1%c /17., -23., 29./
  data b1 / 2.,  -3.,  5.,  -7., 11., -13./

  c1 = matmul(a1(1,:)%c, b1)
  if (any (c1-[248., -749.] /= 0.)) call abort

  c2 = matmul(a1%c, b1)
  if (any (c2-reshape([248., -749.],shape(c2)) /= 0.)) call abort
end program main

! { dg-final { scan-tree-dump-times "_gfortran_matmul" 0 "optimized" } }
