! This can fail because BB is not resolved correctly.
module M1

INTEGER p

CONTAINS
subroutine AA ()
   implicit NONE
   p = BB ()
 CONTAINS
   subroutine AA_1 ()
     implicit NONE
     integer :: i
     i = BB ()
   end subroutine

   function BB()
   integer :: BB
     BB = 1
   end function
end subroutine 

function BB()
  implicit NONE
  integer :: BB
  BB = 2
end function
end module

program P1
  USE M1
  implicit none
  p = 0
  call AA ()
  if (p /= 1) call abort
end
