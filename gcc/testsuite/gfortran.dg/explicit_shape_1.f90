! { dg-do compile }
! PR fortran/83633
! Original testcase by Nathan T. Weeks  <weeks at iastate dot edu>
!
integer :: A(command_argument_count()) = 1 ! { dg-error "nonconstant bounds" }
write (*,*) A
end
