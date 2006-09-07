!Program to test SET_EXPONENT intrinsic function.

program test_set_exponent
  call test_real4()
  call test_real8()
end

subroutine test_real4()
  real*4 x,y
  integer*4 i,n
  equivalence(x, i)

  n = -148
  x = 1024.0
  y = set_exponent (x, n)
  if ((y .ne. 0.0) .and. (exponent (y) .ne. n)) call abort()

  n = 8
  x = 1024.0
  y = set_exponent (x, n)
  if (exponent (y) .ne. n) call abort()

  n = 128
  i = 8388607
  x = transfer (i, x) ! z'007fffff' Positive denormalized floating-point.
  y = set_exponent (x, n)
  if (exponent (y) .ne. n) call abort()

  n = -148
  x = -1024.0
  y = set_exponent (x, n)
  if  ((y .ne. 0.0) .and. (exponent (y) .ne. n)) call abort()

  n = 8
  x = -1024.0
  y = set_exponent (x, n)
  if (y .ne. -128.0) call abort()
  if (exponent (y) .ne. n) call abort()

  n = 128
  i = -2139095041
  x = transfer (i, x) ! z'807fffff' Negative denormalized floating-point.
  y = set_exponent (x, n)
  if (exponent (y) .ne. n) call abort()

end

subroutine test_real8()
  implicit none
  real*8 x, y
  integer*8 i, n
  equivalence(x, i)

  n = -1073
  x = 1024.0_8
  y = set_exponent (x, n)
  if  ((y .ne. 0.0_8) .and. (exponent (y) .ne. n)) call abort()

  n = 8
  x = 1024.0_8
  y = set_exponent (x, n)
  if (y .ne. 128.0) call abort()
  if (exponent (y) .ne. n) call abort()

  n = 1024
  i = 4503599627370495_8
  x = transfer (i, x) !z'000fffffffffffff' Positive denormalized floating-point.
  y = set_exponent (x, n)
  if (exponent (y) .ne. n) call abort()

  n = -1073
  x = -1024.0
  y = set_exponent (x, n)
  if ((y .ne. 0.0) .and. (exponent (y) .ne. n)) call abort()

  n = 8
  x = -1024.0
  y = set_exponent (x, n)
  if (y .ne. -128.0) call abort()
  if (exponent (y) .ne. n) call abort()

  n = 1024
  i = -9218868437227405313_8
  x = transfer (i, x)!z'800fffffffffffff' Negative denormalized floating-point.
  y = set_exponent (x, n)
  if (exponent (y) .ne. n) call abort()
end
