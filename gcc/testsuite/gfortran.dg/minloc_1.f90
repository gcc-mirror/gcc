! { dg-do run }
! { dg-add-options ieee }
! { dg-skip-if "NaN not supported" { spu-*-* } }
  real :: a(3), nan, minf, pinf
  integer :: ia(1)
  real, allocatable :: c(:)
  logical :: l
  logical :: l2(3)

  nan = 0.0
  minf = 0.0
  pinf = 0.0
  nan = 0.0/nan
  minf = -1.0/minf
  pinf = 1.0/pinf

  allocate (c(3))
  a(:) = nan
  ia = minloc (a)
  if (ia(1).ne.1) STOP 1
  a(:) = pinf
  ia = minloc (a)
  if (ia(1).ne.1) STOP 2
  a(1:2) = nan
  ia = minloc (a)
  if (ia(1).ne.3) STOP 3
  a(2) = 1.0
  ia = minloc (a)
  if (ia(1).ne.2) STOP 4
  a(2) = minf
  ia = minloc (a)
  if (ia(1).ne.2) STOP 5
  c(:) = nan
  ia = minloc (c)
  if (ia(1).ne.1) STOP 6
  c(:) = pinf
  ia = minloc (c)
  if (ia(1).ne.1) STOP 7
  c(1:2) = nan
  ia = minloc (c)
  if (ia(1).ne.3) STOP 8
  c(2) = 1.0
  ia = minloc (c)
  if (ia(1).ne.2) STOP 9
  c(2) = minf
  ia = minloc (c)
  if (ia(1).ne.2) STOP 10
  l = .false.
  l2(:) = .false.
  a(:) = nan
  ia = minloc (a, mask = l)
  if (ia(1).ne.0) STOP 11
  ia = minloc (a, mask = l2)
  if (ia(1).ne.0) STOP 12
  a(:) = pinf
  ia = minloc (a, mask = l)
  if (ia(1).ne.0) STOP 13
  ia = minloc (a, mask = l2)
  if (ia(1).ne.0) STOP 14
  a(1:2) = nan
  ia = minloc (a, mask = l)
  if (ia(1).ne.0) STOP 15
  ia = minloc (a, mask = l2)
  if (ia(1).ne.0) STOP 16
  a(2) = 1.0
  ia = minloc (a, mask = l)
  if (ia(1).ne.0) STOP 17
  ia = minloc (a, mask = l2)
  if (ia(1).ne.0) STOP 18
  a(2) = minf
  ia = minloc (a, mask = l)
  if (ia(1).ne.0) STOP 19
  ia = minloc (a, mask = l2)
  if (ia(1).ne.0) STOP 20
  c(:) = nan
  ia = minloc (c, mask = l)
  if (ia(1).ne.0) STOP 21
  ia = minloc (c, mask = l2)
  if (ia(1).ne.0) STOP 22
  c(:) = pinf
  ia = minloc (c, mask = l)
  if (ia(1).ne.0) STOP 23
  ia = minloc (c, mask = l2)
  if (ia(1).ne.0) STOP 24
  c(1:2) = nan
  ia = minloc (c, mask = l)
  if (ia(1).ne.0) STOP 25
  ia = minloc (c, mask = l2)
  if (ia(1).ne.0) STOP 26
  c(2) = 1.0
  ia = minloc (c, mask = l)
  if (ia(1).ne.0) STOP 27
  ia = minloc (c, mask = l2)
  if (ia(1).ne.0) STOP 28
  c(2) = minf
  ia = minloc (c, mask = l)
  if (ia(1).ne.0) STOP 29
  ia = minloc (c, mask = l2)
  if (ia(1).ne.0) STOP 30
  l = .true.
  l2(:) = .true.
  a(:) = nan
  ia = minloc (a, mask = l)
  if (ia(1).ne.1) STOP 31
  ia = minloc (a, mask = l2)
  if (ia(1).ne.1) STOP 32
  a(:) = pinf
  ia = minloc (a, mask = l)
  if (ia(1).ne.1) STOP 33
  ia = minloc (a, mask = l2)
  if (ia(1).ne.1) STOP 34
  a(1:2) = nan
  ia = minloc (a, mask = l)
  if (ia(1).ne.3) STOP 35
  ia = minloc (a, mask = l2)
  if (ia(1).ne.3) STOP 36
  a(2) = 1.0
  ia = minloc (a, mask = l)
  if (ia(1).ne.2) STOP 37
  ia = minloc (a, mask = l2)
  if (ia(1).ne.2) STOP 38
  a(2) = minf
  ia = minloc (a, mask = l)
  if (ia(1).ne.2) STOP 39
  ia = minloc (a, mask = l2)
  if (ia(1).ne.2) STOP 40
  c(:) = nan
  ia = minloc (c, mask = l)
  if (ia(1).ne.1) STOP 41
  ia = minloc (c, mask = l2)
  if (ia(1).ne.1) STOP 42
  c(:) = pinf
  ia = minloc (c, mask = l)
  if (ia(1).ne.1) STOP 43
  ia = minloc (c, mask = l2)
  if (ia(1).ne.1) STOP 44
  c(1:2) = nan
  ia = minloc (c, mask = l)
  if (ia(1).ne.3) STOP 45
  ia = minloc (c, mask = l2)
  if (ia(1).ne.3) STOP 46
  c(2) = 1.0
  ia = minloc (c, mask = l)
  if (ia(1).ne.2) STOP 47
  ia = minloc (c, mask = l2)
  if (ia(1).ne.2) STOP 48
  c(2) = minf
  ia = minloc (c, mask = l)
  if (ia(1).ne.2) STOP 49
  ia = minloc (c, mask = l2)
  if (ia(1).ne.2) STOP 50
  deallocate (c)
  allocate (c(-2:-3))
  ia = minloc (c)
  if (ia(1).ne.0) STOP 51
end
