! Program to test intrinsic functions as actual arguments
subroutine test_r(fn, val, res)
  real fn
  real val, res

  if (diff(fn(val), res)) call abort
contains
function diff(a, b)
  real a, b
  logical diff
  diff = (abs(a - b) .gt. 0.00001)
end function
end subroutine

subroutine test_d(fn, val, res)
  double precision fn
  double precision val, res

  if (diff(fn(val), res)) call abort
contains
function diff(a, b)
  double precision a, b
  logical diff
  diff = (abs(a - b) .gt. 0.00001d0)
end function
end subroutine

subroutine test_r2(fn, val1, val2, res)
  real fn
  real val1, val2, res

  if (diff(fn(val1, val2), res)) call abort
contains
function diff(a, b)
  real a, b
  logical diff
  diff = (abs(a - b) .gt. 0.00001)
end function
end subroutine

subroutine test_d2(fn, val1, val2, res)
  double precision fn
  double precision val1, val2, res

  if (diff(fn(val1, val2), res)) call abort
contains
function diff(a, b)
  double precision a, b
  logical diff
  diff = (abs(a - b) .gt. 0.00001d0)
end function
end subroutine

subroutine test_dprod(fn)
  double precision fn
  if (abs (fn (2.0, 3.0) - 6d0) .gt. 0.00001) call abort
end subroutine

program specifics
  intrinsic abs
  intrinsic aint
  intrinsic anint
  intrinsic acos
  intrinsic asin
  intrinsic atan
  intrinsic cos
  intrinsic sin
  intrinsic tan
  intrinsic cosh
  intrinsic sinh
  intrinsic tanh
  intrinsic alog
  intrinsic exp
  intrinsic sign
  intrinsic amod

  intrinsic dabs
  intrinsic dint
  intrinsic dnint
  intrinsic dacos
  intrinsic dasin
  intrinsic datan
  intrinsic dcos
  intrinsic dsin
  intrinsic dtan
  intrinsic dcosh
  intrinsic dsinh
  intrinsic dtanh
  intrinsic dlog
  intrinsic dexp
  intrinsic dsign
  intrinsic dmod

  intrinsic dprod

  !TODO: Also test complex variants

  call test_r (abs, -1.0, abs(-1.0))
  call test_r (aint, 1.7, 1.0)
  call test_r (anint, 1.7, 2.0)
  call test_r (acos, 0.5, acos(0.5))
  call test_r (asin, 0.5, asin(0.5))
  call test_r (atan, 0.5, atan(0.5))
  call test_r (cos, 1.0, cos(1.0))
  call test_r (sin, 1.0, sin(1.0))
  call test_r (tan, 1.0, tan(1.0))
  call test_r (cosh, 1.0, cosh(1.0))
  call test_r (sinh, 1.0, sinh(1.0))
  call test_r (tanh, 1.0, tanh(1.0))
  call test_r (alog, 2.0, alog(2.0))
  call test_r (exp, 1.0, exp(1.0))
  call test_r2 (sign, 1.0, -2.0, sign(1.0, -2.0))
  call test_r2 (amod, 3.5, 2.0, amod(3.5, 2.0))
  
  call test_d (dabs, -1d0, abs(-1d0))
  call test_d (dint, 1.7d0, 1d0)
  call test_d (dnint, 1.7d0, 2d0)
  call test_d (dacos, 0.5d0, dacos(0.5d0))
  call test_d (dasin, 0.5d0, dasin(0.5d0))
  call test_d (datan, 0.5d0, datan(0.5d0))
  call test_d (dcos, 1d0, dcos(1d0))
  call test_d (dsin, 1d0, dsin(1d0))
  call test_d (dtan, 1d0, dtan(1d0))
  call test_d (dcosh, 1d0, dcosh(1d0))
  call test_d (dsinh, 1d0, dsinh(1d0))
  call test_d (dtanh, 1d0, dtanh(1d0))
  call test_d (dlog, 2d0, dlog(2d0))
  call test_d (dexp, 1d0, dexp(1d0))
  call test_d2 (dsign, 1d0, -2d0, sign(1d0, -2d0))
  call test_d2 (dmod, 3.5d0, 2d0, dmod(3.5d0, 2d0))

  call test_dprod(dprod)
end program

