! { dg-do run }
! { dg-set-target-env-var GFORTRAN_NUM_IMAGES "4" }
program main
  implicit none
  integer, dimension(4):: a[*]
  integer, dimension(4) :: rd
  character (len=16) :: line
  a(:)[this_image()] = 42 + this_image()
  write (unit=line,fmt= '(*(I4))') a(:)[this_image()]
  read (unit=line,fmt=*) rd
  if (any (rd /= 42 + this_image())) stop 1
end program
