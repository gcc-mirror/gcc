! { dg-do run }
! { dg-options "-std=gnu" }
  integer(kind=4) :: i4, j4
  integer(kind=8) :: i8, j8

  i4 = mclock()
  i8 = mclock8()
  j4 = mclock()
  j8 = mclock8()

  if (i4 > j4 .or. i8 > j8 .or. i4 > i8 .or. j4 > j8) call abort

  end
