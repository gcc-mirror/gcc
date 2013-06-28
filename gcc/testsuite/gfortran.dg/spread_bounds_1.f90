! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "Incorrect extent in return value of SPREAD intrinsic in dimension 2: is 3, should be 2" }
program main
  integer :: source(2), target(2,3)
  data source /1,2/
  integer :: times
  times = 2
  target = spread(source,2,times)
end program main
! { dg-output "Fortran runtime error: Incorrect extent in return value of SPREAD intrinsic in dimension 2: is 3, should be 2" }

