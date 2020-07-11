! { dg-do run }
! PR fortran/94578
! This used to give wrong results.  Original test case by Jan-Willem
! Blokland.
program main
  implicit none
  type foo
     integer :: x, y
  end type foo
  integer :: i
  integer, dimension (2,2) :: array2d
  integer, dimension(:), pointer :: array1d
  type(foo), dimension(2*2), target :: solution
  data array2d /1,2,3,4/
  array1d => solution%x
  array1d = reshape (source=array2d, shape=shape(array1d))
  if (any (array1d /= [1,2,3,4])) stop 1
end program main
