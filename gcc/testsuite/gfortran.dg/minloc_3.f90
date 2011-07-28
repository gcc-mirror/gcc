  real :: a(30), m
  real, allocatable :: c(:)
  integer :: e(30), n, ia(1)
  integer, allocatable :: g(:)
  logical :: l(30)
  allocate (c (30))
  allocate (g (30))
  a = 7.0
  c = 7.0
  e = 7
  g = 7
  m = huge(m)
  n = huge(n)
  a(7) = 6.0
  c(7) = 6.0
  e(7) = 6
  g(7) = 6
  ia = minloc (a)
  if (ia(1).ne.7) call abort
  ia = minloc (a(::2))
  if (ia(1).ne.4) call abort
  if (any (minloc (a).ne.(/ 7 /))) call abort
  if (any (minloc (a(::2)).ne.(/ 4 /))) call abort
  ia = minloc (c)
  if (ia(1).ne.7) call abort
  ia = minloc (c(::2))
  if (ia(1).ne.4) call abort
  if (any (minloc (c).ne.(/ 7 /))) call abort
  if (any (minloc (c(::2)).ne.(/ 4 /))) call abort
  ia = minloc (e)
  if (ia(1).ne.7) call abort
  ia = minloc (e(::2))
  if (ia(1).ne.4) call abort
  if (any (minloc (e).ne.(/ 7 /))) call abort
  if (any (minloc (e(::2)).ne.(/ 4 /))) call abort
  ia = minloc (g)
  if (ia(1).ne.7) call abort
  ia = minloc (g(::2))
  if (ia(1).ne.4) call abort
  if (any (minloc (g).ne.(/ 7 /))) call abort
  if (any (minloc (g(::2)).ne.(/ 4 /))) call abort
  l = .true.
  ia = minloc (a, mask = l)
  if (ia(1).ne.7) call abort
  ia = minloc (a(::2), mask = l(::2))
  if (ia(1).ne.4) call abort
  if (any (minloc (a, mask = l).ne.(/ 7 /))) call abort
  if (any (minloc (a(::2), mask = l(::2)).ne.(/ 4 /))) call abort
  ia = minloc (c, mask = l)
  if (ia(1).ne.7) call abort
  ia = minloc (c(::2), mask = l(::2))
  if (ia(1).ne.4) call abort
  if (any (minloc (c, mask = l).ne.(/ 7 /))) call abort
  if (any (minloc (c(::2), mask = l(::2)).ne.(/ 4 /))) call abort
  ia = minloc (e, mask = l)
  if (ia(1).ne.7) call abort
  ia = minloc (e(::2), mask = l(::2))
  if (ia(1).ne.4) call abort
  if (any (minloc (e, mask = l).ne.(/ 7 /))) call abort
  if (any (minloc (e(::2), mask = l(::2)).ne.(/ 4 /))) call abort
  ia = minloc (g, mask = l)
  if (ia(1).ne.7) call abort
  ia = minloc (g(::2), mask = l(::2))
  if (ia(1).ne.4) call abort
  if (any (minloc (g, mask = l).ne.(/ 7 /))) call abort
  if (any (minloc (g(::2), mask = l(::2)).ne.(/ 4 /))) call abort
  l = .false.
  ia = minloc (a, mask = l)
  if (ia(1).ne.0) call abort
  ia = minloc (a(::2), mask = l(::2))
  if (ia(1).ne.0) call abort
  if (any (minloc (a, mask = l).ne.(/ 0 /))) call abort
  if (any (minloc (a(::2), mask = l(::2)).ne.(/ 0 /))) call abort
  ia = minloc (c, mask = l)
  if (ia(1).ne.0) call abort
  ia = minloc (c(::2), mask = l(::2))
  if (ia(1).ne.0) call abort
  if (any (minloc (c, mask = l).ne.(/ 0 /))) call abort
  if (any (minloc (c(::2), mask = l(::2)).ne.(/ 0 /))) call abort
  ia = minloc (e, mask = l)
  if (ia(1).ne.0) call abort
  ia = minloc (e(::2), mask = l(::2))
  if (ia(1).ne.0) call abort
  if (any (minloc (e, mask = l).ne.(/ 0 /))) call abort
  if (any (minloc (e(::2), mask = l(::2)).ne.(/ 0 /))) call abort
  ia = minloc (g, mask = l)
  if (ia(1).ne.0) call abort
  ia = minloc (g(::2), mask = l(::2))
  if (ia(1).ne.0) call abort
  if (any (minloc (g, mask = l).ne.(/ 0 /))) call abort
  if (any (minloc (g(::2), mask = l(::2)).ne.(/ 0 /))) call abort
  a = 7.0
  c = 7.0
end
