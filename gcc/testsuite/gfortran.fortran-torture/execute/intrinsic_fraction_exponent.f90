!Program to test EXPONENT and FRACTION intrinsic function.

program test_exponent_fraction
  real x
  integer*4 i
  real*8 y
  integer*8 j
  equivalence (x, i), (y, j)

  x = 3.
  call test_4(x)

  x = 0.
  call test_4(x)

  i = o'00000000001'
  call test_4(x)

  i = o'00010000000'
  call test_4(x)

  i = o'17700000000'
  call test_4(x)

  i = o'00004000001'
  call test_4(x)

  i = o'17737777777'
  call test_4(x)

  i = o'10000000000'
  call test_4(x)

  i = o'0000010000'
  call test_4(x)

  y = 0.5
  call test_8(y)

  y = 0.
  call test_8(y)

  j = o'00000000001'
  call test_8(y)

  y = 0.2938735877D-38
  call test_8(y)

  y = -1.469369D-39
  call test_8(y)

  y = z'7fe00000'
  call test_8(y)

  y = -5.739719D+42
  call test_8(y)
end

subroutine test_4(x)
real*4 x,y
integer z
y = fraction (x)
z = exponent(x)
if (z .gt. 0) then
  y = (y * 2.) * (2. ** (z - 1))
else
  y = (y / 2.) * (2. ** (z + 1))
end if
if (abs (x - y) .gt. spacing (max (abs (x), abs (y)))) call abort()
end

subroutine test_8(x)
real*8 x, y
integer z
y = fraction (x)
z = exponent(x)
if (z .gt. 0) then
  y = (y * 2._8) * (2._8 ** (z - 1))
else
  y = (y / 2._8) * (2._8 ** (z + 1))
end if
if (abs (x - y) .gt. spacing (max (abs (x), abs(y)))) call abort()
end

