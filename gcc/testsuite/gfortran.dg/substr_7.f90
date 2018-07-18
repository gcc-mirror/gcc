! { dg-do  run }
! PR 70068 - used to allocate too much memory
! Original test cases by Gerhard Steinmetz
program p
   integer :: i
   character(3), parameter :: x(3) = ['abc', 'ijk', 'xyz']
   character(3) :: y(2)
   character(99), parameter :: x2(2) = ' '
   character(99), parameter :: y2=x(2)(99:1)
   y = [(x(i)(i:1), i=2,3)]
   if (any(y /= '')) stop 1
   if (y2 /= '') stop 2
end
