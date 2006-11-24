! Program to test intrinsic functions as actual arguments
!
! Copied from gfortran.fortran-torture/execute/specifics.f90
! Please keep them in sync
!
! It is run here with -ff2c option
!
! { dg-do run }
! { dg-options "-ff2c" }
! Program to test intrinsic functions as actual arguments
subroutine test_c(fn, val, res)
  complex fn
  complex val, res

  if (diff(fn(val),res)) call abort
contains
function diff(a,b)
  complex a,b
  logical diff
  diff = (abs(a - b) .gt. 0.00001)
end function
end subroutine 

subroutine test_z(fn, val, res)
  double complex fn
  double complex val, res

  if (diff(fn(val),res)) call abort
contains
function diff(a,b)
  double complex a,b
  logical diff
  diff = (abs(a - b) .gt. 0.00001)
end function
end subroutine 

subroutine test_cabs(fn, val, res)
  real fn, res
  complex val

  if (diff(fn(val),res)) call abort
contains
function diff(a,b)
  real a,b
  logical diff
  diff = (abs(a - b) .gt. 0.00001)
end function
end subroutine 

subroutine test_cdabs(fn, val, res)
  double precision fn, res
  double complex val

  if (diff(fn(val),res)) call abort
contains
function diff(a,b)
  double precision a,b
  logical diff
  diff = (abs(a - b) .gt. 0.00001)
end function
end subroutine 

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

subroutine test_nint(fn,val,res)
  integer fn, res
  real val
  if (res .ne. fn(val)) call abort
end subroutine

subroutine test_idnint(fn,val,res)
  integer fn, res
  double precision val
  if (res .ne. fn(val)) call abort
end subroutine

subroutine test_idim(fn,val1,val2,res)
  integer fn, res, val1, val2
  if (res .ne. fn(val1,val2)) call abort
end subroutine

subroutine test_iabs(fn,val,res)
  integer fn, res, val
  if (res .ne. fn(val)) call abort
end subroutine

subroutine test_len(fn,val,res)
  integer fn, res
  character(len=*) val
  if (res .ne. fn(val)) call abort
end subroutine

subroutine test_index(fn,val1,val2,res)
  integer fn, res
  character(len=*) val1, val2
  if (fn(val1,val2) .ne. res) call abort
end subroutine

program specifics
  intrinsic abs
  intrinsic aint
  intrinsic anint
  intrinsic acos
  intrinsic acosh
  intrinsic asin
  intrinsic asinh
  intrinsic atan
  intrinsic atanh
  intrinsic cos
  intrinsic sin
  intrinsic tan
  intrinsic cosh
  intrinsic sinh
  intrinsic tanh
  intrinsic alog
  intrinsic alog10
  intrinsic exp
  intrinsic sign
  intrinsic isign
  intrinsic amod

  intrinsic dabs
  intrinsic dint
  intrinsic dnint
  intrinsic dacos
  intrinsic dacosh
  intrinsic dasin
  intrinsic dasinh
  intrinsic datan
  intrinsic datanh
  intrinsic dcos
  intrinsic dsin
  intrinsic dtan
  intrinsic dcosh
  intrinsic dsinh
  intrinsic dtanh
  intrinsic dlog
  intrinsic dlog10
  intrinsic dexp
  intrinsic dsign
  intrinsic dmod

  intrinsic conjg
  intrinsic ccos
  intrinsic cexp
  intrinsic clog
  intrinsic csin
  intrinsic csqrt

  intrinsic dconjg
  intrinsic cdcos
  intrinsic cdexp
  intrinsic cdlog
  intrinsic cdsin
  intrinsic cdsqrt
  intrinsic zcos
  intrinsic zexp
  intrinsic zlog
  intrinsic zsin
  intrinsic zsqrt

  intrinsic cabs
  intrinsic cdabs
  intrinsic zabs

  intrinsic dprod

  intrinsic nint
  intrinsic idnint
  intrinsic dim
  intrinsic ddim
  intrinsic idim
  intrinsic iabs
  intrinsic mod
  intrinsic len
  intrinsic index

  intrinsic aimag
  intrinsic dimag

  call test_r (abs, -1.0, abs(-1.0))
  call test_r (aint, 1.7, aint(1.7))
  call test_r (anint, 1.7, anint(1.7))
  call test_r (acos, 0.5, acos(0.5))
  call test_r (acosh, 1.5, acosh(1.5))
  call test_r (asin, 0.5, asin(0.5))
  call test_r (asinh, 0.5, asinh(0.5))
  call test_r (atan, 0.5, atan(0.5))
  call test_r (atanh, 0.5, atanh(0.5))
  call test_r (cos, 1.0, cos(1.0))
  call test_r (sin, 1.0, sin(1.0))
  call test_r (tan, 1.0, tan(1.0))
  call test_r (cosh, 1.0, cosh(1.0))
  call test_r (sinh, 1.0, sinh(1.0))
  call test_r (tanh, 1.0, tanh(1.0))
  call test_r (alog, 2.0, alog(2.0))
  call test_r (alog10, 2.0, alog10(2.0))
  call test_r (exp, 1.0, exp(1.0))
  call test_r2 (sign, 1.0, -2.0, sign(1.0, -2.0))
  call test_r2 (amod, 3.5, 2.0, amod(3.5, 2.0))
  
  call test_d (dabs, -1d0, abs(-1d0))
  call test_d (dint, 1.7d0, 1d0)
  call test_d (dnint, 1.7d0, 2d0)
  call test_d (dacos, 0.5d0, dacos(0.5d0))
  call test_d (dacosh, 1.5d0, dacosh(1.5d0))
  call test_d (dasin, 0.5d0, dasin(0.5d0))
  call test_d (dasinh, 0.5d0, dasinh(0.5d0))
  call test_d (datan, 0.5d0, datan(0.5d0))
  call test_d (datanh, 0.5d0, datanh(0.5d0))
  call test_d (dcos, 1d0, dcos(1d0))
  call test_d (dsin, 1d0, dsin(1d0))
  call test_d (dtan, 1d0, dtan(1d0))
  call test_d (dcosh, 1d0, dcosh(1d0))
  call test_d (dsinh, 1d0, dsinh(1d0))
  call test_d (dtanh, 1d0, dtanh(1d0))
  call test_d (dlog, 2d0, dlog(2d0))
  call test_d (dlog10, 2d0, dlog10(2d0))
  call test_d (dexp, 1d0, dexp(1d0))
  call test_d2 (dsign, 1d0, -2d0, sign(1d0, -2d0))
  call test_d2 (dmod, 3.5d0, 2d0, dmod(3.5d0, 2d0))

  call test_dprod (dprod)

  call test_c (conjg, (1.2,-4.), conjg((1.2,-4.)))
  call test_c (ccos, (1.2,-4.), ccos((1.2,-4.)))
  call test_c (cexp, (1.2,-4.), cexp((1.2,-4.)))
  call test_c (clog, (1.2,-4.), clog((1.2,-4.)))
  call test_c (csin, (1.2,-4.), csin((1.2,-4.)))
  call test_c (csqrt, (1.2,-4.), csqrt((1.2,-4.)))

  call test_z (dconjg, (1.2d0,-4.d0), dconjg((1.2d0,-4.d0)))
  call test_z (cdcos, (1.2d0,-4.d0), cdcos((1.2d0,-4.d0)))
  call test_z (zcos, (1.2d0,-4.d0), zcos((1.2d0,-4.d0)))
  call test_z (cdexp, (1.2d0,-4.d0), cdexp((1.2d0,-4.d0)))
  call test_z (zexp, (1.2d0,-4.d0), zexp((1.2d0,-4.d0)))
  call test_z (cdlog, (1.2d0,-4.d0), cdlog((1.2d0,-4.d0)))
  call test_z (zlog, (1.2d0,-4.d0), zlog((1.2d0,-4.d0)))
  call test_z (cdsin, (1.2d0,-4.d0), cdsin((1.2d0,-4.d0)))
  call test_z (zsin, (1.2d0,-4.d0), zsin((1.2d0,-4.d0)))
  call test_z (cdsqrt, (1.2d0,-4.d0), cdsqrt((1.2d0,-4.d0)))
  call test_z (zsqrt, (1.2d0,-4.d0), zsqrt((1.2d0,-4.d0)))

  call test_cabs (cabs, (1.2,-4.), cabs((1.2,-4.)))
  call test_cdabs (cdabs, (1.2d0,-4.d0), cdabs((1.2d0,-4.d0)))
  call test_cdabs (zabs, (1.2d0,-4.d0), zabs((1.2d0,-4.d0)))
  call test_cabs (aimag, (1.2,-4.), aimag((1.2,-4.)))
  call test_cdabs (dimag, (1.2d0,-4.d0), dimag((1.2d0,-4.d0)))

  call test_nint (nint, -1.2, nint(-1.2))
  call test_idnint (idnint, -1.2d0, idnint(-1.2d0))
  call test_idim (isign, -42, 17, isign(-42, 17))
  call test_idim (idim, -42, 17, idim(-42,17))
  call test_idim (idim, 42, 17, idim(42,17))
  call test_r2 (dim, 1.2, -4., dim(1.2, -4.))
  call test_d2 (ddim, 1.2d0, -4.d0, ddim(1.2d0, -4.d0))
  call test_iabs (iabs, -7, iabs(-7))
  call test_idim (mod, 5, 2, mod(5,2))
  call test_len (len, "foobar", len("foobar"))
  call test_index (index, "foobarfoobar", "bar", index("foobarfoobar","bar"))

end program

