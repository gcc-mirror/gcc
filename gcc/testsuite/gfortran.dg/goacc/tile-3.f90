! { dg-do compile }
!
! PR fortran/93825
!
! Check that implicit typing works

program p
   !$acc parallel loop tile(2,2)
   do i = 1, 8
      do j = 1, 8
      end do
   end do
end
