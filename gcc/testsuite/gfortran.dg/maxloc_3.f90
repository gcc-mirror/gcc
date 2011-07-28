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
  if (ia(1).ne.1) call abort
  a(2) = huge(h)
  ia = maxloc (a)
  if (ia(1).ne.2) call abort
  a(:) = h
  ia = maxloc (a)
  if (ia(1).ne.1) call abort
  a(3) = -huge(h)
  ia = maxloc (a)
  if (ia(1).ne.3) call abort
  c(:) = 5
  ia = maxloc (c)
  if (ia(1).ne.1) call abort
  c(2) = huge(h)
  ia = maxloc (c)
  if (ia(1).ne.2) call abort
  c(:) = h
  ia = maxloc (c)
  if (ia(1).ne.1) call abort
  c(3) = -huge(h)
  ia = maxloc (c)
  if (ia(1).ne.3) call abort
  l = .false.
  l2(:) = .false.
  a(:) = 5
  ia = maxloc (a, mask = l)
  if (ia(1).ne.0) call abort
  ia = maxloc (a, mask = l2)
  if (ia(1).ne.0) call abort
  a(2) = huge(h)
  ia = maxloc (a, mask = l)
  if (ia(1).ne.0) call abort
  ia = maxloc (a, mask = l2)
  if (ia(1).ne.0) call abort
  a(:) = h
  ia = maxloc (a, mask = l)
  if (ia(1).ne.0) call abort
  ia = maxloc (a, mask = l2)
  if (ia(1).ne.0) call abort
  a(3) = -huge(h)
  ia = maxloc (a, mask = l)
  if (ia(1).ne.0) call abort
  ia = maxloc (a, mask = l2)
  if (ia(1).ne.0) call abort
  c(:) = 5
  ia = maxloc (c, mask = l)
  if (ia(1).ne.0) call abort
  ia = maxloc (c, mask = l2)
  if (ia(1).ne.0) call abort
  c(2) = huge(h)
  ia = maxloc (c, mask = l)
  if (ia(1).ne.0) call abort
  ia = maxloc (c, mask = l2)
  if (ia(1).ne.0) call abort
  c(:) = h
  ia = maxloc (c, mask = l)
  if (ia(1).ne.0) call abort
  ia = maxloc (c, mask = l2)
  if (ia(1).ne.0) call abort
  c(3) = -huge(h)
  ia = maxloc (c, mask = l)
  if (ia(1).ne.0) call abort
  ia = maxloc (c, mask = l2)
  if (ia(1).ne.0) call abort
  l = .true.
  l2(:) = .true.
  a(:) = 5
  ia = maxloc (a, mask = l)
  if (ia(1).ne.1) call abort
  ia = maxloc (a, mask = l2)
  if (ia(1).ne.1) call abort
  a(2) = huge(h)
  ia = maxloc (a, mask = l)
  if (ia(1).ne.2) call abort
  ia = maxloc (a, mask = l2)
  if (ia(1).ne.2) call abort
  a(:) = h
  ia = maxloc (a, mask = l)
  if (ia(1).ne.1) call abort
  ia = maxloc (a, mask = l2)
  if (ia(1).ne.1) call abort
  a(3) = -huge(h)
  ia = maxloc (a, mask = l)
  if (ia(1).ne.3) call abort
  ia = maxloc (a, mask = l2)
  if (ia(1).ne.3) call abort
  c(:) = 5
  ia = maxloc (c, mask = l)
  if (ia(1).ne.1) call abort
  ia = maxloc (c, mask = l2)
  if (ia(1).ne.1) call abort
  c(2) = huge(h)
  ia = maxloc (c, mask = l)
  if (ia(1).ne.2) call abort
  ia = maxloc (c, mask = l2)
  if (ia(1).ne.2) call abort
  c(:) = h
  ia = maxloc (c, mask = l)
  if (ia(1).ne.1) call abort
  ia = maxloc (c, mask = l2)
  if (ia(1).ne.1) call abort
  c(3) = -huge(h)
  ia = maxloc (c, mask = l)
  if (ia(1).ne.3) call abort
  ia = maxloc (c, mask = l2)
  if (ia(1).ne.3) call abort
  deallocate (c)
  allocate (c(-2:-3))
  ia = maxloc (c)
  if (ia(1).ne.0) call abort
end
