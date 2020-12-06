! { dg-do run }
! { dg-set-target-env-var GFORTRAN_NUM_IMAGES "2" }
program main
  implicit none
  if (num_images() /= 2) stop 1
end program main

