! { dg-do run }
! Testcase for SIGN() with integer arguments
! Check that:
!   + SIGN() evaluates its arguments only once
!   + SIGN() works on large values
!   + SIGN() works with parameter arguments
! Contributed by FX Coudert <fxcoudert@gmail.com>
program sign1
  implicit none
  integer(kind=1), parameter :: one1 = 1_1, mone1 = -1_1
  integer(kind=2), parameter :: one2 = 1_2, mone2 = -1_2
  integer(kind=4), parameter :: one4 = 1_4, mone4 = -1_4
  integer(kind=8), parameter :: one8 = 1_8, mone8 = -1_8
  integer(kind=1) :: i1, j1
  integer(kind=2) :: i2, j2
  integer(kind=4) :: i4, j4
  integer(kind=8) :: i8, j8
  integer :: i = 1

  i1 = huge(0_1) ; j1 = -huge(0_1)
  if (sign(i1, j1) /= j1) STOP 1
  if (sign(j1, i1) /= i1) STOP 2
  if (sign(i1,one1) /= i1 .or. sign(j1,one1) /= i1) STOP 3
  if (sign(i1,mone1) /= j1 .or. sign(j1,mone1) /= j1) STOP 4

  i2 = huge(0_2) ; j2 = -huge(0_2)
  if (sign(i2, j2) /= j2) STOP 5
  if (sign(j2, i2) /= i2) STOP 6
  if (sign(i2,one2) /= i2 .or. sign(j2,one2) /= i2) STOP 7
  if (sign(i2,mone2) /= j2 .or. sign(j2,mone2) /= j2) STOP 8

  i4 = huge(0_4) ; j4 = -huge(0_4)
  if (sign(i4, j4) /= j4) STOP 9
  if (sign(j4, i4) /= i4) STOP 10
  if (sign(i4,one4) /= i4 .or. sign(j4,one4) /= i4) STOP 11
  if (sign(i4,mone4) /= j4 .or. sign(j4,mone4) /= j4) STOP 12

  i8 = huge(0_8) ; j8 = -huge(0_8)
  if (sign(i8, j8) /= j8) STOP 13
  if (sign(j8, i8) /= i8) STOP 14
  if (sign(i8,one8) /= i8 .or. sign(j8,one8) /= i8) STOP 15
  if (sign(i8,mone8) /= j8 .or. sign(j8,mone8) /= j8) STOP 16

  if (sign(foo(i), 1) /= 1) STOP 1
  if (sign(foo(i), -1) /= -2) STOP 2
  if (sign(42, foo(i)) /= 42) STOP 3
  if (sign(42, -foo(i)) /= -42) STOP 4
  if (i /= 5) STOP 5

  if (sign(bar(), 1) /= 1) STOP 6
  if (sign(bar(), -1) /= -2) STOP 7
  if (sign(17, bar()) /= 17) STOP 8
  if (sign(17, -bar()) /= -17) STOP 9
  if (bar() /= 5) STOP 10

contains

  integer function foo(i)
    integer :: i
    foo = i
    i = i + 1
  end function

  integer function bar()
    integer, save :: i = 0
    i = i + 1
    bar = i
  end function
end
