!Program to test SCALE intrinsic function.

program test_scale
  call test_real4 (3.0, 2)
  call test_real4 (33.0, -2)
  call test_real4 (-3., 2)
  call test_real4 (0., 3)
  call test_real8 (0._8, 3)
  call test_real8 (3.0_8, 4)
  call test_real8 (33.0_8, -4)
  call test_real8 (-33._8, 4)
end
subroutine test_real4 (orig, i)
  real x,y,orig
  integer i
  x = orig
  y = x * (2.0 ** i)
  x = scale (x, i)
  if (abs (x - y) .gt. abs(x * 1e-6)) STOP 1
end

subroutine test_real8 (orig, i)
  real*8 x,y,orig
  integer i
  x = orig
  y = x * (2.0 ** i)
  x = scale (x, i)
  if (abs (x - y) .gt. abs(x * 1e-6)) STOP 2
end
