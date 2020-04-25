! { dg-do run }
! PR fortran/94578
! This used to give wrong results.
program main
  implicit none
  type foo
     integer :: x, y,z
  end type foo
  integer :: i
  integer, dimension(:), pointer :: array1d
  type(foo), dimension(2), target :: solution
  integer, dimension(2,2) :: a
  data a /1,2,3,4/
  solution%x = -10
  solution%y = -20
  array1d => solution%x
  array1d = maxval(a,dim=1)
  if (any (array1d /= [2,4])) stop 1
end program main
