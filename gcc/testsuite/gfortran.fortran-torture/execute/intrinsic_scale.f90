!Program to test SCALE intrinsic function.

program test_scale
  call test_real4 (3.0, 2)
  call test_real4 (33.0, -2)
  call test_real4 (-3., 2)
  call test_real4 (0, 3)
  call test_real8 (0, 3)
  call test_real8 (3.0_8, 4)
  call test_real8 (33.0_8, -4)
  call test_real8 (-33._8, 4)
end
subroutine test_real4 (x, i)
  real x,y
  integer i
  y = x * (2.0 ** i)
  x = scale (x, i)
  if (abs (x - y) .gt. abs(x * 1e-6)) call abort
end

subroutine test_real8 (x, i)
  real*8 x,y
  integer i
  y = x * (2.0 ** i)
  x = scale (x, i)
  if (abs (x - y) .gt. abs(x * 1e-6)) call abort
end
