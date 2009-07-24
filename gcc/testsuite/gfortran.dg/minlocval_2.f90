! { dg-do run }
  integer :: a(3), h
  integer, allocatable :: c(:)
  logical :: l
  logical :: l2(3)

  h = -huge(h)
  h = h - 1
  allocate (c(3))
  a(:) = 5
  if (minloc (a, dim = 1).ne.1) call abort
  if (minval (a, dim = 1).ne.5) call abort
  a(2) = h
  if (minloc (a, dim = 1).ne.2) call abort
  if (minval (a, dim = 1).ne.h) call abort
  a(:) = huge(h)
  if (minloc (a, dim = 1).ne.1) call abort
  if (minval (a, dim = 1).ne.huge(h)) call abort
  a(3) = huge(h) - 1
  if (minloc (a, dim = 1).ne.3) call abort
  if (minval (a, dim = 1).ne.huge(h)-1) call abort
  c(:) = 5
  if (minloc (c, dim = 1).ne.1) call abort
  if (minval (c, dim = 1).ne.5) call abort
  c(2) = h
  if (minloc (c, dim = 1).ne.2) call abort
  if (minval (c, dim = 1).ne.h) call abort
  c(:) = huge(h)
  if (minloc (c, dim = 1).ne.1) call abort
  if (minval (c, dim = 1).ne.huge(h)) call abort
  c(3) = huge(h) - 1
  if (minloc (c, dim = 1).ne.3) call abort
  if (minval (c, dim = 1).ne.huge(h)-1) call abort
  l = .false.
  l2(:) = .false.
  a(:) = 5
  if (minloc (a, dim = 1, mask = l).ne.0) call abort
  if (minval (a, dim = 1, mask = l).ne.huge(h)) call abort
  if (minloc (a, dim = 1, mask = l2).ne.0) call abort
  if (minval (a, dim = 1, mask = l2).ne.huge(h)) call abort
  a(2) = h
  if (minloc (a, dim = 1, mask = l).ne.0) call abort
  if (minval (a, dim = 1, mask = l).ne.huge(h)) call abort
  if (minloc (a, dim = 1, mask = l2).ne.0) call abort
  if (minval (a, dim = 1, mask = l2).ne.huge(h)) call abort
  a(:) = huge(h)
  if (minloc (a, dim = 1, mask = l).ne.0) call abort
  if (minval (a, dim = 1, mask = l).ne.huge(h)) call abort
  if (minloc (a, dim = 1, mask = l2).ne.0) call abort
  if (minval (a, dim = 1, mask = l2).ne.huge(h)) call abort
  a(3) = huge(h) - 1
  if (minloc (a, dim = 1, mask = l).ne.0) call abort
  if (minval (a, dim = 1, mask = l).ne.huge(h)) call abort
  if (minloc (a, dim = 1, mask = l2).ne.0) call abort
  if (minval (a, dim = 1, mask = l2).ne.huge(h)) call abort
  c(:) = 5
  if (minloc (c, dim = 1, mask = l).ne.0) call abort
  if (minval (c, dim = 1, mask = l).ne.huge(h)) call abort
  if (minloc (c, dim = 1, mask = l2).ne.0) call abort
  if (minval (c, dim = 1, mask = l2).ne.huge(h)) call abort
  c(2) = h
  if (minloc (c, dim = 1, mask = l).ne.0) call abort
  if (minval (c, dim = 1, mask = l).ne.huge(h)) call abort
  if (minloc (c, dim = 1, mask = l2).ne.0) call abort
  if (minval (c, dim = 1, mask = l2).ne.huge(h)) call abort
  c(:) = huge(h)
  if (minloc (c, dim = 1, mask = l).ne.0) call abort
  if (minval (c, dim = 1, mask = l).ne.huge(h)) call abort
  if (minloc (c, dim = 1, mask = l2).ne.0) call abort
  if (minval (c, dim = 1, mask = l2).ne.huge(h)) call abort
  c(3) = huge(h) - 1
  if (minloc (c, dim = 1, mask = l).ne.0) call abort
  if (minval (c, dim = 1, mask = l).ne.huge(h)) call abort
  if (minloc (c, dim = 1, mask = l2).ne.0) call abort
  if (minval (c, dim = 1, mask = l2).ne.huge(h)) call abort
  l = .true.
  l2(:) = .true.
  a(:) = 5
  if (minloc (a, dim = 1, mask = l).ne.1) call abort
  if (minval (a, dim = 1, mask = l).ne.5) call abort
  if (minloc (a, dim = 1, mask = l2).ne.1) call abort
  if (minval (a, dim = 1, mask = l2).ne.5) call abort
  a(2) = h
  if (minloc (a, dim = 1, mask = l).ne.2) call abort
  if (minval (a, dim = 1, mask = l).ne.h) call abort
  if (minloc (a, dim = 1, mask = l2).ne.2) call abort
  if (minval (a, dim = 1, mask = l2).ne.h) call abort
  a(:) = huge(h)
  if (minloc (a, dim = 1, mask = l).ne.1) call abort
  if (minval (a, dim = 1, mask = l).ne.huge(h)) call abort
  if (minloc (a, dim = 1, mask = l2).ne.1) call abort
  if (minval (a, dim = 1, mask = l2).ne.huge(h)) call abort
  a(3) = huge(h) - 1
  if (minloc (a, dim = 1, mask = l).ne.3) call abort
  if (minval (a, dim = 1, mask = l).ne.huge(h)-1) call abort
  if (minloc (a, dim = 1, mask = l2).ne.3) call abort
  if (minval (a, dim = 1, mask = l2).ne.huge(h)-1) call abort
  c(:) = 5
  if (minloc (c, dim = 1, mask = l).ne.1) call abort
  if (minval (c, dim = 1, mask = l).ne.5) call abort
  if (minloc (c, dim = 1, mask = l2).ne.1) call abort
  if (minval (c, dim = 1, mask = l2).ne.5) call abort
  c(2) = h
  if (minloc (c, dim = 1, mask = l).ne.2) call abort
  if (minval (c, dim = 1, mask = l).ne.h) call abort
  if (minloc (c, dim = 1, mask = l2).ne.2) call abort
  if (minval (c, dim = 1, mask = l2).ne.h) call abort
  c(:) = huge(h)
  if (minloc (c, dim = 1, mask = l).ne.1) call abort
  if (minval (c, dim = 1, mask = l).ne.huge(h)) call abort
  if (minloc (c, dim = 1, mask = l2).ne.1) call abort
  if (minval (c, dim = 1, mask = l2).ne.huge(h)) call abort
  c(3) = huge(h) - 1
  if (minloc (c, dim = 1, mask = l).ne.3) call abort
  if (minval (c, dim = 1, mask = l).ne.huge(h)-1) call abort
  if (minloc (c, dim = 1, mask = l2).ne.3) call abort
  if (minval (c, dim = 1, mask = l2).ne.huge(h)-1) call abort
  deallocate (c)
  allocate (c(-2:-3))
  if (minloc (c, dim = 1).ne.0) call abort
  if (minval (c, dim = 1).ne.huge(h)) call abort
end
