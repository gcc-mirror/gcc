!
! { dg-do run }
!
! PR fortran/119836
!
program p
   implicit none
   integer, parameter :: n = 4
   integer :: i
   integer :: y(n), x(n)
   do concurrent (i=1:n)
      x(i) = shiftl (i,1)     ! accepted
      block
         y(i) = shiftl (i,1)  ! wrongly rejected
      end block
   end do
   if (any(x /= y)) stop 1
end program p
