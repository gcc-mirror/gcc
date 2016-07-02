! PR fortran/71705
! { dg-do compile }

  real :: x
  x = 0.0
  !$omp target update to(x)
end
