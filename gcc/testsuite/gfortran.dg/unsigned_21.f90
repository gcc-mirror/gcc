! { dg-do run }
! { dg-options "-funsigned" }
program main
  integer :: i
  integer(2) :: j
  unsigned :: u
  i = -1
  u = transfer(i,u)
  if (u /= huge(u)) error stop 1
  u = 40000u
  j = transfer(u,j)
  if (j /= -25536) error stop 2
end program main
