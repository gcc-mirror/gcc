! { dg-do compile }
! { dg-options "-fall-intrinsics -std=f95" }
! PR fortran/20248
program z
   if (iargc() /= 0) call abort
end program z
