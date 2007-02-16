! { dg-do run }
! NOT() was not return the two's complement value as reported by
! PR fortran/25458.  In checking other bit manipulation intrinsics,
! IBSET was found to be in error.
program chkbits

  implicit none

  integer(kind=1) i1
  integer(kind=2) i2
  integer(kind=4) i4
  integer(kind=8) i8

  i1 = ibset(huge(0_1), bit_size(i1)-1)
  i2 = ibset(huge(0_2), bit_size(i2)-1)
  i4 = ibset(huge(0_4), bit_size(i4)-1)
  i8 = ibset(huge(0_8), bit_size(i8)-1)
  if (i1 /= -1 .or. i2 /= -1 .or. i4 /= -1 .or. i8 /= -1) call abort

  i1 = ibclr(-1_1, bit_size(i1)-1)
  i2 = ibclr(-1_2, bit_size(i2)-1)
  i4 = ibclr(-1_4, bit_size(i4)-1)
  i8 = ibclr(-1_8, bit_size(i8)-1)
  if (i1 /= huge(0_1) .or. i2 /= huge(0_2)) call abort
  if (i4 /= huge(0_4) .or. i8 /= huge(0_8)) call abort

  i1 = not(0_1)
  i2 = not(0_2)
  i4 = not(0_4)
  i8 = not(0_8)
  if (i1 /= -1 .or. i2 /= -1 .or. i4 /= -1 .or. i8 /= -1) call abort

end program chkbits
