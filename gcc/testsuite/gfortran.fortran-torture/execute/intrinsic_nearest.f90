!Program to test NEAREST intrinsic function.

program test_nearest
  real s, r, x, y, inf, max
  integer i, infi, maxi
  equivalence (s,i)
  equivalence (inf,infi)
  equivalence (max,maxi)

  r = 2.0
  s = 3.0
  call test_n (s, r)

  i = int(z'00800000')
  call test_n (s, r)

  i = int(z'007fffff')
  call test_n (s, r)

  i = int(z'00800100')
  call test_n (s, r)

  s = 0
  x = nearest(s, r)
  y = nearest(s, -r)
  if (.not. (x .gt. s .and. y .lt. s )) STOP 1

  infi = int(z'7f800000')
  maxi = int(z'7f7fffff')

  call test_up(max, inf)
  call test_up(-inf, -max)
  call test_down(inf, max)
  call test_down(-max, -inf)

! ??? Here we require the F2003 IEEE_ARITHMETIC module to
! determine if denormals are supported.  If they are, then
! nearest(0,1) is the minimum denormal.  If they are not,
! then it's the minimum normalized number, TINY.  This fails
! much more often than the infinity test above, so it's
! disabled for now.

! call test_up(0, min)
! call test_up(-min, 0)
! call test_down(0, -min)
! call test_down(min, 0)
end

subroutine test_up(s, e)
  real s, e, x

  x = nearest(s, 1.0)
  if (x .ne. e) STOP 2
end

subroutine test_down(s, e)
  real s, e, x

  x = nearest(s, -1.0)
  if (x .ne. e) STOP 3
end

subroutine test_n(s1, r)
  real r, s1, x

  x = nearest(s1, r)
  if (nearest(x, -r) .ne. s1) STOP 4
  x = nearest(s1, -r)
  if (nearest(x, r) .ne. s1) STOP 5

  s1 = -s1
  x = nearest(s1, r)
  if (nearest(x, -r) .ne. s1) STOP 6
  x = nearest(s1, -r)
  if (nearest(x, r) .ne. s1) STOP 7
end
