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

  i1 = ibset(2147483647,bit_size(i4)-1)
  i2 = ibset(2147483647,bit_size(i4)-1)
  i4 = ibset(2147483647,bit_size(i4)-1)
  i8 = ibset(2147483647,bit_size(i4)-1)
  if (i1 /= -1 .or. i2 /= -1 .or. i4 /= -1 .or. i8 /= -1) call abort

  i1 = not(0)
  i2 = not(0)
  i4 = not(0)
  i8 = not(0)
  if (i1 /= -1 .or. i2 /= -1 .or. i4 /= -1 .or. i8 /= -1) call abort

end program chkbits
