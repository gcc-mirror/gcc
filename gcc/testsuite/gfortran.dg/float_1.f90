! { dg-do run }
! PR fortran/26816
program test_float
   integer(1) :: i1 = 1
   integer(2) :: i2 = 1
   integer(4) :: i4 = 1
   integer(8) :: i8 = 1
   if (float(i1) /= 1.) call abort
   if (float(i2) /= 1.) call abort
   if (float(i4) /= 1.) call abort
   if (float(i8) /= 1.) call abort
end program test_float
