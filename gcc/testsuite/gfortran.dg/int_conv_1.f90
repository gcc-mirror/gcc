! { dg-do run }
! { dg-options "-std=gnu" }
  integer(kind=2) :: i2, j2, k2, l2, m2, n2, o2
  integer(kind=4) :: i4, j4
  integer(kind=8) :: i8, j8
  real :: x
  complex :: z

  i2 = huge(i2) / 3
  i8 = int8(i2)
  i4 = long(i2)
  j2 = short(i2)
  k2 = int2(i2)
  l2 = int2(i8)
  m2 = short(i8)
  n2 = int2(i4)
  o2 = short(i4)

  if (i8 /= i2 .or. i4 /= i2 .or. j2 /= i2 .or. k2 /= i2 &
      .or. l2 /= i2 .or. m2 /= i2 .or. n2 /= i2 .or. o2 /= i2) call abort

  x = i2
  i8 = int8(x)
  i4 = long(x)
  j2 = short(x)
  k2 = int2(x)
  if (i8 /= i2 .or. i4 /= i2 .or. j2 /= i2 .or. k2 /= i2) call abort

  z = i2 + (0.,-42.)
  i8 = int8(z)
  i4 = long(z)
  j2 = short(z)
  k2 = int2(z)
  if (i8 /= i2 .or. i4 /= i2 .or. j2 /= i2 .or. k2 /= i2) call abort

  end
