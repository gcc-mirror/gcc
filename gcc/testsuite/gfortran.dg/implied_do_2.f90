! { dg-do compile }
! PR fortran/56667
program error_message
   implicit none
   integer :: ir
   write(*,*) ( ir, ir = 1,10    ! { dg-error "Expected a right parenthesis" }
end program error_message 
