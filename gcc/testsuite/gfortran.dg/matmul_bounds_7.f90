! { dg-do run }
! { dg-additional-options "-fcheck=bounds" }
! { dg-shouldfail "Fortran runtime error: dimension of array B incorrect in MATMUL intrinsic" }

program main
  real, dimension(3,2) :: a
  real, dimension(6) :: b
  real, dimension(:), allocatable :: c

  data a /-2., 3., -5., 7., -11., 13./
  data b /17., -23., 29., -31., 37., -41./

  c = matmul(pack(b,[b<20.]),a)
  print *,sum(c)

end program main
