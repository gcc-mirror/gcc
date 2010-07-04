! { dg-do run }
! Progam to test the dfloat intrinsic.
program dfloat_1
   implicit none
   integer(2) i2
   integer(4) i4
   integer(8) i8
   i2 = -4_2
   i4 = 4_4
   i8 = 10_8
   if (dfloat(i2) /= -4.d0) call abort()       ! { dg-warning "non-default INTEGER" }
   if (dfloat(i4) /= 4.d0) call abort()
   if (dfloat(i8) /= 10.d0) call abort()       ! { dg-warning "non-default INTEGER" }
   if (dfloat(i4*i2) /= -16.d0) call abort()

   if (kind(dfloat(i4)) /= kind(1.0_8)) call abort
   if (kind(dfloat(i8)) /= kind(1.0_8)) call abort   ! { dg-warning "non-default INTEGER" }
end program dfloat_1
