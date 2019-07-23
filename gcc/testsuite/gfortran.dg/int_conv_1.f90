! { dg-do run }
! { dg-options "-std=gnu" }
  integer(kind=2) :: i2, k2, l2
  integer(kind=8) :: i8
  real :: x
  complex :: z

  i2 = huge(i2) / 3
  i8 = int8(i2)
  k2 = int2(i2)
  l2 = int2(i8)

  if (i8 /= i2 .or. k2 /= i2 .or. l2 /= i2 ) STOP 1

  x = i2
  i8 = int8(x)
  k2 = int2(x)
  if (i8 /= i2 .or. k2 /= i2) STOP 2

  z = i2 + (0.,-42.)
  i8 = int8(z)
  k2 = int2(z)
  if (i8 /= i2 .or. k2 /= i2) STOP 3

  end
