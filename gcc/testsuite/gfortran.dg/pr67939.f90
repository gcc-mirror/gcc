! { dg-do compile }
! PR fortran/67939
! Original code by Gerhard Steinmetz
! gerhard dot steinmetz dot fortran at t-online dot de
!
program p
   character(100) :: x
   data x(998:99) /'ab'/   ! { dg-warning "Unused initialization string" }
   call a
end

subroutine a
   character(2) :: x
   data x(:-1) /'ab'/      ! { dg-warning "Unused initialization string" }
end subroutine a

subroutine b
   character(8) :: x
   data x(3:1) /'abc'/     ! { dg-warning "Unused initialization string" }
end subroutine b

