!Program to test SPACING intrinsic function.

program test_spacing
  call test_real4(3.0)
  call test_real4(33.0)
  call test_real4(-3.)
  call test_real4(0.0)
  call test_real8(0.0_8)
  call test_real8(3.0_8)
  call test_real8(33.0_8)
  call test_real8(-33._8)
end
subroutine test_real4(orig)
  real x,y,t,orig
  integer p
  x = orig
  p = 24
  y = 2.0 ** (exponent (x) - p)
  t = tiny(x)
  x = spacing(x)
  if ((abs (x - y) .gt. abs(x * 1e-6)) &
    .and. (abs (x - t) .gt. abs(x * 1e-6)))call abort
end

subroutine test_real8(orig)
  real*8 x,y,t,orig
  integer p
  x = orig
  p = 53
  y = 2.0 ** (exponent (x) - p)
  t = tiny (x)
  x = spacing(x)
  if ((abs (x - y) .gt. abs(x * 1e-6)) &
    .and. (abs (x - t) .gt. abs(x * 1e-6)))call abort
end
