! { dg-do compile }
! { dg-options "-std=legacy" }
! PR 91550 - this used to cause an ICE
! Test case by Gerhard Steinmetz
program p
   real :: a(3)
   integer :: i
   do i = 1, 3, .1 ! { dg-error "cannot be zero" }
      a(i) = i
   end do
end
