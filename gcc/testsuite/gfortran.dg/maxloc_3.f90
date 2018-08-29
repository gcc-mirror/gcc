! { dg-do run }
  integer :: a(3), h, ia(1)
  integer, allocatable :: c(:)
  logical :: l
  logical :: l2(3)

  h = -huge(h)
  h = h - 1
  allocate (c(3))
  a(:) = 5
  ia = maxloc (a)
  if (ia(1).ne.1) STOP 1
  a(2) = huge(h)
  ia = maxloc (a)
  if (ia(1).ne.2) STOP 2
  a(:) = h
  ia = maxloc (a)
  if (ia(1).ne.1) STOP 3
  a(3) = -huge(h)
  ia = maxloc (a)
  if (ia(1).ne.3) STOP 4
  c(:) = 5
  ia = maxloc (c)
  if (ia(1).ne.1) STOP 5
  c(2) = huge(h)
  ia = maxloc (c)
  if (ia(1).ne.2) STOP 6
  c(:) = h
  ia = maxloc (c)
  if (ia(1).ne.1) STOP 7
  c(3) = -huge(h)
  ia = maxloc (c)
  if (ia(1).ne.3) STOP 8
  l = .false.
  l2(:) = .false.
  a(:) = 5
  ia = maxloc (a, mask = l)
  if (ia(1).ne.0) STOP 9
  ia = maxloc (a, mask = l2)
  if (ia(1).ne.0) STOP 10
  a(2) = huge(h)
  ia = maxloc (a, mask = l)
  if (ia(1).ne.0) STOP 11
  ia = maxloc (a, mask = l2)
  if (ia(1).ne.0) STOP 12
  a(:) = h
  ia = maxloc (a, mask = l)
  if (ia(1).ne.0) STOP 13
  ia = maxloc (a, mask = l2)
  if (ia(1).ne.0) STOP 14
  a(3) = -huge(h)
  ia = maxloc (a, mask = l)
  if (ia(1).ne.0) STOP 15
  ia = maxloc (a, mask = l2)
  if (ia(1).ne.0) STOP 16
  c(:) = 5
  ia = maxloc (c, mask = l)
  if (ia(1).ne.0) STOP 17
  ia = maxloc (c, mask = l2)
  if (ia(1).ne.0) STOP 18
  c(2) = huge(h)
  ia = maxloc (c, mask = l)
  if (ia(1).ne.0) STOP 19
  ia = maxloc (c, mask = l2)
  if (ia(1).ne.0) STOP 20
  c(:) = h
  ia = maxloc (c, mask = l)
  if (ia(1).ne.0) STOP 21
  ia = maxloc (c, mask = l2)
  if (ia(1).ne.0) STOP 22
  c(3) = -huge(h)
  ia = maxloc (c, mask = l)
  if (ia(1).ne.0) STOP 23
  ia = maxloc (c, mask = l2)
  if (ia(1).ne.0) STOP 24
  l = .true.
  l2(:) = .true.
  a(:) = 5
  ia = maxloc (a, mask = l)
  if (ia(1).ne.1) STOP 25
  ia = maxloc (a, mask = l2)
  if (ia(1).ne.1) STOP 26
  a(2) = huge(h)
  ia = maxloc (a, mask = l)
  if (ia(1).ne.2) STOP 27
  ia = maxloc (a, mask = l2)
  if (ia(1).ne.2) STOP 28
  a(:) = h
  ia = maxloc (a, mask = l)
  if (ia(1).ne.1) STOP 29
  ia = maxloc (a, mask = l2)
  if (ia(1).ne.1) STOP 30
  a(3) = -huge(h)
  ia = maxloc (a, mask = l)
  if (ia(1).ne.3) STOP 31
  ia = maxloc (a, mask = l2)
  if (ia(1).ne.3) STOP 32
  c(:) = 5
  ia = maxloc (c, mask = l)
  if (ia(1).ne.1) STOP 33
  ia = maxloc (c, mask = l2)
  if (ia(1).ne.1) STOP 34
  c(2) = huge(h)
  ia = maxloc (c, mask = l)
  if (ia(1).ne.2) STOP 35
  ia = maxloc (c, mask = l2)
  if (ia(1).ne.2) STOP 36
  c(:) = h
  ia = maxloc (c, mask = l)
  if (ia(1).ne.1) STOP 37
  ia = maxloc (c, mask = l2)
  if (ia(1).ne.1) STOP 38
  c(3) = -huge(h)
  ia = maxloc (c, mask = l)
  if (ia(1).ne.3) STOP 39
  ia = maxloc (c, mask = l2)
  if (ia(1).ne.3) STOP 40
  deallocate (c)
  allocate (c(-2:-3))
  ia = maxloc (c)
  if (ia(1).ne.0) STOP 41
end
