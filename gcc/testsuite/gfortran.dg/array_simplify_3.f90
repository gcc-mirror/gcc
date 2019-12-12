! { dg-do  run }
! PR 71203 - this used to ICE
program p
   integer :: i
   integer, parameter :: x(2) = 0
   integer, parameter :: y(*) = [(x(i:i), i=1,2)]
   if (size(y,1) /= 2) stop 1
   if (any(y /= 0)) stop 2
end
