!Program to test NEAREST intrinsic function.

program test_nearest
  real s, r, x, y, inf, max, min
  integer i, infi, maxi, mini
  equivalence (s,i)
  equivalence (inf,infi)
  equivalence (max,maxi)
  equivalence (min,mini)

  r = 2.0
  s = 3.0
  call test_n (s, r)

  i = z'00800000'
  call test_n (s, r)

  i = z'007fffff'
  call test_n (s, r)

  i = z'00800100'
  call test_n (s, r)

  s = 0
  x = nearest(s, r)
  y = nearest(s, -r)
  if (.not. (x .gt. s .and. y .lt. s )) call abort()

  infi = z'7f800000'
  maxi = z'7f7fffff'
  mini = 1

  call test_up(max, inf)
  call test_up(-inf, -max)
  call test_up(0, min)
  call test_up(-min, 0)

  call test_down(inf, max)
  call test_down(-max, -inf)
  call test_down(0, -min)
  call test_down(min, 0)
end

subroutine test_up(s, e)
  real s, e, x

  x = nearest(s, 1.0)
  if (x .ne. e) call abort()
end

subroutine test_down(s, e)
  real s, e, x

  x = nearest(s, -1.0)
  if (x .ne. e) call abort()
end

subroutine test_n(s1, r)
  real r, s1, x

  x = nearest(s1, r)
  if (nearest(x, -r) .ne. s1) call abort()
  x = nearest(s1, -r)
  if (nearest(x, r) .ne. s1) call abort()

  s1 = -s1
  x = nearest(s1, r)
  if (nearest(x, -r) .ne. s1) call abort()
  x = nearest(s1, -r)
  if (nearest(x, r) .ne. s1) call abort()
end
