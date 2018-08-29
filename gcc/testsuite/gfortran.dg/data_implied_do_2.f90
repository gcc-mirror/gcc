! { dg-do compile }
! PR fortran/84134 - this used to ICE.
! Test case by Gerhard Steinmetz

program p
   integer :: i, x(3)
   data (x(i+1:i+2:i),i=0,1) /1,2,3/ ! { dg-error "Nonconstant array section" }
end
