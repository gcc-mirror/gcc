! { dg-do run }
! { dg-set-target-env-var GFORTRAN_NUM_IMAGES "4" }
program main
  sync images(*)
end program main
