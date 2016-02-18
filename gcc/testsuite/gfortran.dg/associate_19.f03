! { dg-do run }
!
! Contributed by mrestelli@gmail.com
! Adapated by Andre Vehreschild  <vehre@gcc.gnu.org>
! Test that fix for PR69296 is working.

program p
 implicit none

 integer :: j, a(2,6), i(3,2)

  a(1,:) = (/ (     j , j=1,6) /)
  a(2,:) = (/ ( -10*j , j=1,6) /)

  i(:,1) = (/ 1 , 3 , 5 /)
  i(:,2) = (/ 4 , 5 , 6 /)

  associate( ai => a(:,i(:,1)) )
    if (any(shape(ai) /= [2, 3])) call abort()
    if (any(reshape(ai, [6]) /= [1 , -10, 3, -30, 5, -50])) call abort()
  end associate

end program p
