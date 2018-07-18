! { dg-do run }
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
  if (ia(1).ne.7) STOP 1
  ia = minloc (a(::2))
  if (ia(1).ne.4) STOP 2
  if (any (minloc (a).ne.(/ 7 /))) STOP 3
  if (any (minloc (a(::2)).ne.(/ 4 /))) STOP 4
  ia = minloc (c)
  if (ia(1).ne.7) STOP 5
  ia = minloc (c(::2))
  if (ia(1).ne.4) STOP 6
  if (any (minloc (c).ne.(/ 7 /))) STOP 7
  if (any (minloc (c(::2)).ne.(/ 4 /))) STOP 8
  ia = minloc (e)
  if (ia(1).ne.7) STOP 9
  ia = minloc (e(::2))
  if (ia(1).ne.4) STOP 10
  if (any (minloc (e).ne.(/ 7 /))) STOP 11
  if (any (minloc (e(::2)).ne.(/ 4 /))) STOP 12
  ia = minloc (g)
  if (ia(1).ne.7) STOP 13
  ia = minloc (g(::2))
  if (ia(1).ne.4) STOP 14
  if (any (minloc (g).ne.(/ 7 /))) STOP 15
  if (any (minloc (g(::2)).ne.(/ 4 /))) STOP 16
  l = .true.
  ia = minloc (a, mask = l)
  if (ia(1).ne.7) STOP 17
  ia = minloc (a(::2), mask = l(::2))
  if (ia(1).ne.4) STOP 18
  if (any (minloc (a, mask = l).ne.(/ 7 /))) STOP 19
  if (any (minloc (a(::2), mask = l(::2)).ne.(/ 4 /))) STOP 20
  ia = minloc (c, mask = l)
  if (ia(1).ne.7) STOP 21
  ia = minloc (c(::2), mask = l(::2))
  if (ia(1).ne.4) STOP 22
  if (any (minloc (c, mask = l).ne.(/ 7 /))) STOP 23
  if (any (minloc (c(::2), mask = l(::2)).ne.(/ 4 /))) STOP 24
  ia = minloc (e, mask = l)
  if (ia(1).ne.7) STOP 25
  ia = minloc (e(::2), mask = l(::2))
  if (ia(1).ne.4) STOP 26
  if (any (minloc (e, mask = l).ne.(/ 7 /))) STOP 27
  if (any (minloc (e(::2), mask = l(::2)).ne.(/ 4 /))) STOP 28
  ia = minloc (g, mask = l)
  if (ia(1).ne.7) STOP 29
  ia = minloc (g(::2), mask = l(::2))
  if (ia(1).ne.4) STOP 30
  if (any (minloc (g, mask = l).ne.(/ 7 /))) STOP 31
  if (any (minloc (g(::2), mask = l(::2)).ne.(/ 4 /))) STOP 32
  l = .false.
  ia = minloc (a, mask = l)
  if (ia(1).ne.0) STOP 33
  ia = minloc (a(::2), mask = l(::2))
  if (ia(1).ne.0) STOP 34
  if (any (minloc (a, mask = l).ne.(/ 0 /))) STOP 35
  if (any (minloc (a(::2), mask = l(::2)).ne.(/ 0 /))) STOP 36
  ia = minloc (c, mask = l)
  if (ia(1).ne.0) STOP 37
  ia = minloc (c(::2), mask = l(::2))
  if (ia(1).ne.0) STOP 38
  if (any (minloc (c, mask = l).ne.(/ 0 /))) STOP 39
  if (any (minloc (c(::2), mask = l(::2)).ne.(/ 0 /))) STOP 40
  ia = minloc (e, mask = l)
  if (ia(1).ne.0) STOP 41
  ia = minloc (e(::2), mask = l(::2))
  if (ia(1).ne.0) STOP 42
  if (any (minloc (e, mask = l).ne.(/ 0 /))) STOP 43
  if (any (minloc (e(::2), mask = l(::2)).ne.(/ 0 /))) STOP 44
  ia = minloc (g, mask = l)
  if (ia(1).ne.0) STOP 45
  ia = minloc (g(::2), mask = l(::2))
  if (ia(1).ne.0) STOP 46
  if (any (minloc (g, mask = l).ne.(/ 0 /))) STOP 47
  if (any (minloc (g(::2), mask = l(::2)).ne.(/ 0 /))) STOP 48
  a = 7.0
  c = 7.0
end
