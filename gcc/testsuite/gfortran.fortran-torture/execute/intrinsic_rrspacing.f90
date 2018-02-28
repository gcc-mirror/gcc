!Program to test RRSPACING intrinsic function.

program test_rrspacing
  call test_real4(3.0)
  call test_real4(33.0)
  call test_real4(-3.0)
  call test_real8(3.0_8)
  call test_real8(33.0_8)
  call test_real8(-33.0_8)
end
subroutine test_real4(orig)
  real x,y,orig
  integer p
  x = orig
  p = 24
  y = abs (x * 2.0 ** (- exponent (x))) * (2.0 ** p)
  x = rrspacing(x)
  if (abs (x - y) .gt. abs(x * 1e-6)) STOP 1
end

subroutine test_real8(orig)
  real*8 x,y,t,orig
  integer p
  x = orig
  p = 53
  y = abs (x * 2.0 ** (- exponent (x))) * (2.0 ** p)
  x = rrspacing(x)
  if (abs (x - y) .gt. abs(x * 1e-6)) STOP 2
end
