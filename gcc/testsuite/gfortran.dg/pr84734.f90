! { dg-do compile }
! PR fortran/84734
   integer :: b(huge(1_8)+1_8) = 0 ! { dg-error "Arithmetic overflow" }
   end
