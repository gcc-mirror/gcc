! { dg-do run }
! { dg-set-target-env-var GFORTRAN_NUM_IMAGES "4" }
! { dg-output "1234" }
program main
  implicit none
  integer :: n, me
  n = num_images()
  me = this_image()
  if (me /= 1) sync images (me - 1)
  write (*,'(I0)',advance="no") me
  if (me /= n)  sync images (me+1)
  sync all
end program main
