! { dg-do compile }
! PR fortran/66043
!
! Original code from Gerhard Steinmetz
! <gerhard dot steinmetz dot fortran at t-online dot de>
program p
   print *, storage_size(null()) ! { dg-error "cannot be an actual" }
end
