! { dg-do compile }
! { dg-options " -std=f95" }
! PR fortran/20248
program z
   if (iargc() /= 0) STOP 1
end program z
