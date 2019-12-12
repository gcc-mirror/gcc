! { dg-do compile }
! { dg-options "-fallow-invalid-boz" }
! PR fortran/91650
! Code contributed by Gerhard Steinmetz.
program p
   character(len=60) str
   write(str,*) b'10110' ! { dg-warning "cannot appear in an output IO list" }
   if (trim(adjustl(str)) /= '22') stop 1
   write(str,*) o'10110' ! { dg-warning "cannot appear in an output IO list" }
   if (trim(adjustl(str)) /= '4168') stop 2
   write(str,*) z'10110' ! { dg-warning "cannot appear in an output IO list" }
   if (trim(adjustl(str)) /= '65808') stop 3
end
