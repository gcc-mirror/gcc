!Program to test SET_EXPONENT intrinsic function.

program test_set_exponent
  call test_real4()
  call test_real8()
end
subroutine test_real4()
  real x,y
  integer i,n
  equivalence(x,i)

  n = -148
  x = 1024.0
  y = set_exponent (x, n)
  if ((y .ne. 0.0) .and. (exponent (y) .ne. n)) call abort()

  n = 8
  x = 1024.0
  y = set_exponent (x, n)
  if (exponent (y) .ne. n) call abort()

  n = 128
  i = o'00037777777'
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
  i = o'20037777777'
  y = set_exponent (x, n)
  if (exponent (y) .ne. n) call abort()

end

subroutine test_real8()
  implicit none
  real*8 x, y
  integer*8 i, n, low
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
  low = z'ffffffff'
  i = z'000fffff' 
  i = ishft (i, 32) + low !'000fffffffffffff'
  y = set_exponent (x, n)
  low = z'fffffffe'
  i = z'7fefffff' 
  i = ishft (i, 32) + low
  if (exponent (y) .ne. n) call abort()

  n = -1073
  x = -1024.0
  y = set_exponent (x, n)
  low = z'00000001'
  if ((y .ne. 0.0) .and. (exponent (y) .ne. n)) call abort()

  n = 8
  x = -1024.0
  y = set_exponent (x, n)
  if (y .ne. -128.0) call abort()
  if (exponent (y) .ne. n) call abort()

  n = 1024
  low = z'ffffffff'
  i = z'800fffff' 
  i = ishft (i, 32) + low !z'800fffffffffffff'
  y = set_exponent (x, n)
  if (exponent (y) .ne. n) call abort()

end
