! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "Incorrect size in SHIFT argument of CSHIFT intrinsic: should not be zero-sized" }
program main
  real, dimension(1,0) :: a, b, c
  integer :: sp(3), i
  a = 4.0
  sp = 1
  i = 1
  b = cshift (a,sp(1:i)) ! Invalid
end program main
! { dg-output "Fortran runtime error: Incorrect size in SHIFT argument of CSHIFT intrinsic: should not be zero-sized" }
