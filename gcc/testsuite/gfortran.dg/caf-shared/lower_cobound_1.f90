! { dg-do run }
! { dg-set-target-env-var GFORTRAN_NUM_IMAGES "4" }

program main
  implicit none
  integer, parameter :: lower = 64000
  integer, dimension(3) :: a[lower:*]
  character (len=40) :: line1, line2
  integer :: i
  a (1) = lower - 1 + this_image()
  a (2) = 42
  a (3) = 43
  write (unit=line1,fmt='(3I6)') a
  write (unit=line2,fmt='(3I6)') lower - 1 + this_image(), 42, 43
  if (line1 /= line2) stop 1
  sync all
  do i=lower, lower-1+this_image()
     if (a(1)[i] /= i) stop 2
  end do
end program main
