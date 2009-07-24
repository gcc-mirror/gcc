  real :: a(30), b(10, 10), m
  real, allocatable :: c(:), d(:, :)
  integer :: e(30), f(10, 10), n
  integer, allocatable :: g(:), h(:,:)
  logical :: l(30), l2(10, 10)
  allocate (c (30))
  allocate (d (10, 10))
  allocate (g (30))
  allocate (h (10, 10))
  a = 7.0
  b = 7.0
  c = 7.0
  d = 7.0
  e = 7
  f = 7
  g = 7
  h = 7
  m = huge(m)
  n = huge(n)
  a(7) = 6.0
  b(5, 5) = 6.0
  b(5, 6) = 5.0
  b(6, 7) = 4.0
  c(7) = 6.0
  d(5, 5) = 6.0
  d(5, 6) = 5.0
  d(6, 7) = 4.0
  e(7) = 6
  f(5, 5) = 6
  f(5, 6) = 5
  f(6, 7) = 4
  g(7) = 6
  h(5, 5) = 6
  h(5, 6) = 5
  h(6, 7) = 4
  if (minloc (a, dim = 1).ne.7) call abort
  if (minval (a, dim = 1).ne.6.0) call abort
  if (minloc (a(::2), dim = 1).ne.4) call abort
  if (minval (a(::2), dim = 1).ne.6.0) call abort
  if (any (minloc (a).ne.(/ 7 /))) call abort
  if (minval (a).ne.6.0) call abort
  if (any (minloc (a(::2)).ne.(/ 4 /))) call abort
  if (minval (a(::2)).ne.6.0) call abort
  if (any (minloc (b, dim = 1).ne.(/ 1, 1, 1, 1, 5, 5, 6, 1, 1, 1 /))) call abort
  if (any (minval (b, dim = 1).ne.(/ 7.0, 7.0, 7.0, 7.0, 6.0, 5.0, 4.0, 7.0, 7.0, 7.0 /))) call abort
  if (any (minloc (b(::2,::2), dim = 1).ne.(/ 1, 1, 3, 1, 1 /))) call abort
  if (any (minval (b(::2,::2), dim = 1).ne.(/ 7.0, 7.0, 6.0, 7.0, 7.0 /))) call abort
  if (any (minloc (b, dim = 2).ne.(/ 1, 1, 1, 1, 6, 7, 1, 1, 1, 1 /))) call abort
  if (any (minval (b, dim = 2).ne.(/ 7.0, 7.0, 7.0, 7.0, 5.0, 4.0, 7.0, 7.0, 7.0, 7.0 /))) call abort
  if (any (minloc (b(::2,::2), dim = 2).ne.(/ 1, 1, 3, 1, 1 /))) call abort
  if (any (minval (b(::2,::2), dim = 2).ne.(/ 7.0, 7.0, 6.0, 7.0, 7.0 /))) call abort
  if (any (minloc (b).ne.(/ 6, 7 /))) call abort
  if (minval (b).ne.4.0) call abort
  if (any (minloc (b(::2,::2)).ne.(/ 3, 3 /))) call abort
  if (minval (b(::2,::2)).ne.6.0) call abort
  if (minloc (c, dim = 1).ne.7) call abort
  if (minval (c, dim = 1).ne.6.0) call abort
  if (minloc (c(::2), dim = 1).ne.4) call abort
  if (minval (c(::2), dim = 1).ne.6.0) call abort
  if (any (minloc (c).ne.(/ 7 /))) call abort
  if (minval (c).ne.6.0) call abort
  if (any (minloc (c(::2)).ne.(/ 4 /))) call abort
  if (minval (c(::2)).ne.6.0) call abort
  if (any (minloc (d, dim = 1).ne.(/ 1, 1, 1, 1, 5, 5, 6, 1, 1, 1 /))) call abort
  if (any (minval (d, dim = 1).ne.(/ 7.0, 7.0, 7.0, 7.0, 6.0, 5.0, 4.0, 7.0, 7.0, 7.0 /))) call abort
  if (any (minloc (d(::2,::2), dim = 1).ne.(/ 1, 1, 3, 1, 1 /))) call abort
  if (any (minval (d(::2,::2), dim = 1).ne.(/ 7.0, 7.0, 6.0, 7.0, 7.0 /))) call abort
  if (any (minloc (d, dim = 2).ne.(/ 1, 1, 1, 1, 6, 7, 1, 1, 1, 1 /))) call abort
  if (any (minval (d, dim = 2).ne.(/ 7.0, 7.0, 7.0, 7.0, 5.0, 4.0, 7.0, 7.0, 7.0, 7.0 /))) call abort
  if (any (minloc (d(::2,::2), dim = 2).ne.(/ 1, 1, 3, 1, 1 /))) call abort
  if (any (minval (d(::2,::2), dim = 2).ne.(/ 7.0, 7.0, 6.0, 7.0, 7.0 /))) call abort
  if (any (minloc (d).ne.(/ 6, 7 /))) call abort
  if (minval (d).ne.4.0) call abort
  if (any (minloc (d(::2,::2)).ne.(/ 3, 3 /))) call abort
  if (minval (d(::2,::2)).ne.6.0) call abort
  if (minloc (e, dim = 1).ne.7) call abort
  if (minval (e, dim = 1).ne.6) call abort
  if (minloc (e(::2), dim = 1).ne.4) call abort
  if (minval (e(::2), dim = 1).ne.6) call abort
  if (any (minloc (e).ne.(/ 7 /))) call abort
  if (minval (e).ne.6) call abort
  if (any (minloc (e(::2)).ne.(/ 4 /))) call abort
  if (minval (e(::2)).ne.6) call abort
  if (any (minloc (f, dim = 1).ne.(/ 1, 1, 1, 1, 5, 5, 6, 1, 1, 1 /))) call abort
  if (any (minval (f, dim = 1).ne.(/ 7, 7, 7, 7, 6, 5, 4, 7, 7, 7 /))) call abort
  if (any (minloc (f(::2,::2), dim = 1).ne.(/ 1, 1, 3, 1, 1 /))) call abort
  if (any (minval (f(::2,::2), dim = 1).ne.(/ 7, 7, 6, 7, 7 /))) call abort
  if (any (minloc (f, dim = 2).ne.(/ 1, 1, 1, 1, 6, 7, 1, 1, 1, 1 /))) call abort
  if (any (minval (f, dim = 2).ne.(/ 7, 7, 7, 7, 5, 4, 7, 7, 7, 7 /))) call abort
  if (any (minloc (f(::2,::2), dim = 2).ne.(/ 1, 1, 3, 1, 1 /))) call abort
  if (any (minval (f(::2,::2), dim = 2).ne.(/ 7, 7, 6, 7, 7 /))) call abort
  if (any (minloc (f).ne.(/ 6, 7 /))) call abort
  if (minval (f).ne.4) call abort
  if (any (minloc (f(::2,::2)).ne.(/ 3, 3 /))) call abort
  if (minval (f(::2,::2)).ne.6) call abort
  if (minloc (g, dim = 1).ne.7) call abort
  if (minval (g, dim = 1).ne.6) call abort
  if (minloc (g(::2), dim = 1).ne.4) call abort
  if (minval (g(::2), dim = 1).ne.6) call abort
  if (any (minloc (g).ne.(/ 7 /))) call abort
  if (minval (g).ne.6) call abort
  if (any (minloc (g(::2)).ne.(/ 4 /))) call abort
  if (minval (g(::2)).ne.6) call abort
  if (any (minloc (h, dim = 1).ne.(/ 1, 1, 1, 1, 5, 5, 6, 1, 1, 1 /))) call abort
  if (any (minval (h, dim = 1).ne.(/ 7, 7, 7, 7, 6, 5, 4, 7, 7, 7 /))) call abort
  if (any (minloc (h(::2,::2), dim = 1).ne.(/ 1, 1, 3, 1, 1 /))) call abort
  if (any (minval (h(::2,::2), dim = 1).ne.(/ 7, 7, 6, 7, 7 /))) call abort
  if (any (minloc (h, dim = 2).ne.(/ 1, 1, 1, 1, 6, 7, 1, 1, 1, 1 /))) call abort
  if (any (minval (h, dim = 2).ne.(/ 7, 7, 7, 7, 5, 4, 7, 7, 7, 7 /))) call abort
  if (any (minloc (h(::2,::2), dim = 2).ne.(/ 1, 1, 3, 1, 1 /))) call abort
  if (any (minval (h(::2,::2), dim = 2).ne.(/ 7, 7, 6, 7, 7 /))) call abort
  if (any (minloc (h).ne.(/ 6, 7 /))) call abort
  if (minval (h).ne.4) call abort
  if (any (minloc (h(::2,::2)).ne.(/ 3, 3 /))) call abort
  if (minval (h(::2,::2)).ne.6) call abort
  l = .true.
  l2 = .true.
  if (minloc (a, dim = 1, mask = l).ne.7) call abort
  if (minval (a, dim = 1, mask = l).ne.6.0) call abort
  if (minloc (a(::2), dim = 1, mask = l(::2)).ne.4) call abort
  if (minval (a(::2), dim = 1, mask = l(::2)).ne.6.0) call abort
  if (any (minloc (a, mask = l).ne.(/ 7 /))) call abort
  if (minval (a, mask = l).ne.6.0) call abort
  if (any (minloc (a(::2), mask = l(::2)).ne.(/ 4 /))) call abort
  if (minval (a(::2), mask = l(::2)).ne.6.0) call abort
  if (any (minloc (b, dim = 1, mask = l2).ne.(/ 1, 1, 1, 1, 5, 5, 6, 1, 1, 1 /))) call abort
  if (any (minval (b, dim = 1, mask = l2).ne.(/ 7.0, 7.0, 7.0, 7.0, 6.0, 5.0, 4.0, 7.0, 7.0, 7.0 /))) call abort
  if (any (minloc (b(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 1, 1, 3, 1, 1 /))) call abort
  if (any (minval (b(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 7.0, 7.0, 6.0, 7.0, 7.0 /))) call abort
  if (any (minloc (b, dim = 2, mask = l2).ne.(/ 1, 1, 1, 1, 6, 7, 1, 1, 1, 1 /))) call abort
  if (any (minval (b, dim = 2, mask = l2).ne.(/ 7.0, 7.0, 7.0, 7.0, 5.0, 4.0, 7.0, 7.0, 7.0, 7.0 /))) call abort
  if (any (minloc (b(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 1, 1, 3, 1, 1 /))) call abort
  if (any (minval (b(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 7.0, 7.0, 6.0, 7.0, 7.0 /))) call abort
  if (any (minloc (b, mask = l2).ne.(/ 6, 7 /))) call abort
  if (minval (b, mask = l2).ne.4.0) call abort
  if (any (minloc (b(::2,::2), mask = l2(::2,::2)).ne.(/ 3, 3 /))) call abort
  if (minval (b(::2,::2), mask = l2(::2,::2)).ne.6.0) call abort
  if (minloc (c, dim = 1, mask = l).ne.7) call abort
  if (minval (c, dim = 1, mask = l).ne.6.0) call abort
  if (minloc (c(::2), dim = 1, mask = l(::2)).ne.4) call abort
  if (minval (c(::2), dim = 1, mask = l(::2)).ne.6.0) call abort
  if (any (minloc (c, mask = l).ne.(/ 7 /))) call abort
  if (minval (c, mask = l).ne.6.0) call abort
  if (any (minloc (c(::2), mask = l(::2)).ne.(/ 4 /))) call abort
  if (minval (c(::2), mask = l(::2)).ne.6.0) call abort
  if (any (minloc (d, dim = 1, mask = l2).ne.(/ 1, 1, 1, 1, 5, 5, 6, 1, 1, 1 /))) call abort
  if (any (minval (d, dim = 1, mask = l2).ne.(/ 7.0, 7.0, 7.0, 7.0, 6.0, 5.0, 4.0, 7.0, 7.0, 7.0 /))) call abort
  if (any (minloc (d(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 1, 1, 3, 1, 1 /))) call abort
  if (any (minval (d(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 7.0, 7.0, 6.0, 7.0, 7.0 /))) call abort
  if (any (minloc (d, dim = 2, mask = l2).ne.(/ 1, 1, 1, 1, 6, 7, 1, 1, 1, 1 /))) call abort
  if (any (minval (d, dim = 2, mask = l2).ne.(/ 7.0, 7.0, 7.0, 7.0, 5.0, 4.0, 7.0, 7.0, 7.0, 7.0 /))) call abort
  if (any (minloc (d(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 1, 1, 3, 1, 1 /))) call abort
  if (any (minval (d(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 7.0, 7.0, 6.0, 7.0, 7.0 /))) call abort
  if (any (minloc (d, mask = l2).ne.(/ 6, 7 /))) call abort
  if (minval (d, mask = l2).ne.4.0) call abort
  if (any (minloc (d(::2,::2), mask = l2(::2,::2)).ne.(/ 3, 3 /))) call abort
  if (minval (d(::2,::2), mask = l2(::2,::2)).ne.6.0) call abort
  if (minloc (e, dim = 1, mask = l).ne.7) call abort
  if (minval (e, dim = 1, mask = l).ne.6) call abort
  if (minloc (e(::2), dim = 1, mask = l(::2)).ne.4) call abort
  if (minval (e(::2), dim = 1, mask = l(::2)).ne.6) call abort
  if (any (minloc (e, mask = l).ne.(/ 7 /))) call abort
  if (minval (e, mask = l).ne.6) call abort
  if (any (minloc (e(::2), mask = l(::2)).ne.(/ 4 /))) call abort
  if (minval (e(::2), mask = l(::2)).ne.6) call abort
  if (any (minloc (f, dim = 1, mask = l2).ne.(/ 1, 1, 1, 1, 5, 5, 6, 1, 1, 1 /))) call abort
  if (any (minval (f, dim = 1, mask = l2).ne.(/ 7, 7, 7, 7, 6, 5, 4, 7, 7, 7 /))) call abort
  if (any (minloc (f(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 1, 1, 3, 1, 1 /))) call abort
  if (any (minval (f(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 7, 7, 6, 7, 7 /))) call abort
  if (any (minloc (f, dim = 2, mask = l2).ne.(/ 1, 1, 1, 1, 6, 7, 1, 1, 1, 1 /))) call abort
  if (any (minval (f, dim = 2, mask = l2).ne.(/ 7, 7, 7, 7, 5, 4, 7, 7, 7, 7 /))) call abort
  if (any (minloc (f(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 1, 1, 3, 1, 1 /))) call abort
  if (any (minval (f(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 7, 7, 6, 7, 7 /))) call abort
  if (any (minloc (f, mask = l2).ne.(/ 6, 7 /))) call abort
  if (minval (f, mask = l2).ne.4) call abort
  if (any (minloc (f(::2,::2), mask = l2(::2,::2)).ne.(/ 3, 3 /))) call abort
  if (minval (f(::2,::2), mask = l2(::2,::2)).ne.6) call abort
  if (minloc (g, dim = 1, mask = l).ne.7) call abort
  if (minval (g, dim = 1, mask = l).ne.6) call abort
  if (minloc (g(::2), dim = 1, mask = l(::2)).ne.4) call abort
  if (minval (g(::2), dim = 1, mask = l(::2)).ne.6) call abort
  if (any (minloc (g, mask = l).ne.(/ 7 /))) call abort
  if (minval (g, mask = l).ne.6) call abort
  if (any (minloc (g(::2), mask = l(::2)).ne.(/ 4 /))) call abort
  if (minval (g(::2), mask = l(::2)).ne.6) call abort
  if (any (minloc (h, dim = 1, mask = l2).ne.(/ 1, 1, 1, 1, 5, 5, 6, 1, 1, 1 /))) call abort
  if (any (minval (h, dim = 1, mask = l2).ne.(/ 7, 7, 7, 7, 6, 5, 4, 7, 7, 7 /))) call abort
  if (any (minloc (h(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 1, 1, 3, 1, 1 /))) call abort
  if (any (minval (h(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 7, 7, 6, 7, 7 /))) call abort
  if (any (minloc (h, dim = 2, mask = l2).ne.(/ 1, 1, 1, 1, 6, 7, 1, 1, 1, 1 /))) call abort
  if (any (minval (h, dim = 2, mask = l2).ne.(/ 7, 7, 7, 7, 5, 4, 7, 7, 7, 7 /))) call abort
  if (any (minloc (h(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 1, 1, 3, 1, 1 /))) call abort
  if (any (minval (h(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 7, 7, 6, 7, 7 /))) call abort
  if (any (minloc (h, mask = l2).ne.(/ 6, 7 /))) call abort
  if (minval (h, mask = l2).ne.4) call abort
  if (any (minloc (h(::2,::2), mask = l2(::2,::2)).ne.(/ 3, 3 /))) call abort
  if (minval (h(::2,::2), mask = l2(::2,::2)).ne.6) call abort
  l = .false.
  l2 = .false.
  if (minloc (a, dim = 1, mask = l).ne.0) call abort
  if (minval (a, dim = 1, mask = l).ne.m) call abort
  if (minloc (a(::2), dim = 1, mask = l(::2)).ne.0) call abort
  if (minval (a(::2), dim = 1, mask = l(::2)).ne.m) call abort
  if (any (minloc (a, mask = l).ne.(/ 0 /))) call abort
  if (minval (a, mask = l).ne.m) call abort
  if (any (minloc (a(::2), mask = l(::2)).ne.(/ 0 /))) call abort
  if (minval (a(::2), mask = l(::2)).ne.m) call abort
  if (any (minloc (b, dim = 1, mask = l2).ne.(/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /))) call abort
  if (any (minval (b, dim = 1, mask = l2).ne.(/ m, m, m, m, m, m, m, m, m, m /))) call abort
  if (any (minloc (b(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 0, 0, 0, 0, 0 /))) call abort
  if (any (minval (b(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ m, m, m, m, m /))) call abort
  if (any (minloc (b, dim = 2, mask = l2).ne.(/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /))) call abort
  if (any (minval (b, dim = 2, mask = l2).ne.(/ m, m, m, m, m, m, m, m, m, m /))) call abort
  if (any (minloc (b(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 0, 0, 0, 0, 0 /))) call abort
  if (any (minval (b(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ m, m, m, m, m /))) call abort
  if (any (minloc (b, mask = l2).ne.(/ 0, 0 /))) call abort
  if (minval (b, mask = l2).ne.m) call abort
  if (any (minloc (b(::2,::2), mask = l2(::2,::2)).ne.(/ 0, 0 /))) call abort
  if (minval (b(::2,::2), mask = l2(::2,::2)).ne.m) call abort
  if (minloc (c, dim = 1, mask = l).ne.0) call abort
  if (minval (c, dim = 1, mask = l).ne.m) call abort
  if (minloc (c(::2), dim = 1, mask = l(::2)).ne.0) call abort
  if (minval (c(::2), dim = 1, mask = l(::2)).ne.m) call abort
  if (any (minloc (c, mask = l).ne.(/ 0 /))) call abort
  if (minval (c, mask = l).ne.m) call abort
  if (any (minloc (c(::2), mask = l(::2)).ne.(/ 0 /))) call abort
  if (minval (c(::2), mask = l(::2)).ne.m) call abort
  if (any (minloc (d, dim = 1, mask = l2).ne.(/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /))) call abort
  if (any (minval (d, dim = 1, mask = l2).ne.(/ m, m, m, m, m, m, m, m, m, m /))) call abort
  if (any (minloc (d(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 0, 0, 0, 0, 0 /))) call abort
  if (any (minval (d(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ m, m, m, m, m /))) call abort
  if (any (minloc (d, dim = 2, mask = l2).ne.(/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /))) call abort
  if (any (minval (d, dim = 2, mask = l2).ne.(/ m, m, m, m, m, m, m, m, m, m /))) call abort
  if (any (minloc (d(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 0, 0, 0, 0, 0 /))) call abort
  if (any (minval (d(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ m, m, m, m, m /))) call abort
  if (any (minloc (d, mask = l2).ne.(/ 0, 0 /))) call abort
  if (minval (d, mask = l2).ne.m) call abort
  if (any (minloc (d(::2,::2), mask = l2(::2,::2)).ne.(/ 0, 0 /))) call abort
  if (minval (d(::2,::2), mask = l2(::2,::2)).ne.m) call abort
  if (minloc (e, dim = 1, mask = l).ne.0) call abort
  if (minval (e, dim = 1, mask = l).ne.n) call abort
  if (minloc (e(::2), dim = 1, mask = l(::2)).ne.0) call abort
  if (minval (e(::2), dim = 1, mask = l(::2)).ne.n) call abort
  if (any (minloc (e, mask = l).ne.(/ 0 /))) call abort
  if (minval (e, mask = l).ne.n) call abort
  if (any (minloc (e(::2), mask = l(::2)).ne.(/ 0 /))) call abort
  if (minval (e(::2), mask = l(::2)).ne.n) call abort
  if (any (minloc (f, dim = 1, mask = l2).ne.(/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /))) call abort
  if (any (minval (f, dim = 1, mask = l2).ne.(/ n, n, n, n, n, n, n, n, n, n /))) call abort
  if (any (minloc (f(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 0, 0, 0, 0, 0 /))) call abort
  if (any (minval (f(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ n, n, n, n, n /))) call abort
  if (any (minloc (f, dim = 2, mask = l2).ne.(/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /))) call abort
  if (any (minval (f, dim = 2, mask = l2).ne.(/ n, n, n, n, n, n, n, n, n, n /))) call abort
  if (any (minloc (f(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 0, 0, 0, 0, 0 /))) call abort
  if (any (minval (f(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ n, n, n, n, n /))) call abort
  if (any (minloc (f, mask = l2).ne.(/ 0, 0 /))) call abort
  if (minval (f, mask = l2).ne.n) call abort
  if (any (minloc (f(::2,::2), mask = l2(::2,::2)).ne.(/ 0, 0 /))) call abort
  if (minval (f(::2,::2), mask = l2(::2,::2)).ne.n) call abort
  if (minloc (g, dim = 1, mask = l).ne.0) call abort
  if (minval (g, dim = 1, mask = l).ne.n) call abort
  if (minloc (g(::2), dim = 1, mask = l(::2)).ne.0) call abort
  if (minval (g(::2), dim = 1, mask = l(::2)).ne.n) call abort
  if (any (minloc (g, mask = l).ne.(/ 0 /))) call abort
  if (minval (g, mask = l).ne.n) call abort
  if (any (minloc (g(::2), mask = l(::2)).ne.(/ 0 /))) call abort
  if (minval (g(::2), mask = l(::2)).ne.n) call abort
  if (any (minloc (h, dim = 1, mask = l2).ne.(/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /))) call abort
  if (any (minval (h, dim = 1, mask = l2).ne.(/ n, n, n, n, n, n, n, n, n, n /))) call abort
  if (any (minloc (h(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ 0, 0, 0, 0, 0 /))) call abort
  if (any (minval (h(::2,::2), dim = 1, mask = l2(::2,::2)).ne.(/ n, n, n, n, n /))) call abort
  if (any (minloc (h, dim = 2, mask = l2).ne.(/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /))) call abort
  if (any (minval (h, dim = 2, mask = l2).ne.(/ n, n, n, n, n, n, n, n, n, n /))) call abort
  if (any (minloc (h(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ 0, 0, 0, 0, 0 /))) call abort
  if (any (minval (h(::2,::2), dim = 2, mask = l2(::2,::2)).ne.(/ n, n, n, n, n /))) call abort
  if (any (minloc (h, mask = l2).ne.(/ 0, 0 /))) call abort
  if (minval (h, mask = l2).ne.n) call abort
  if (any (minloc (h(::2,::2), mask = l2(::2,::2)).ne.(/ 0, 0 /))) call abort
  if (minval (h(::2,::2), mask = l2(::2,::2)).ne.n) call abort
  a = 7.0
  b = 7.0
  c = 7.0
  d = 7.0
end
