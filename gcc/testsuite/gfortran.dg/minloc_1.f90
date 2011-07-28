! { dg-do run }
! { dg-add-options ieee }
! { dg-skip-if "NaN not supported" { spu-*-* } { "*" } { "" } }
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
  if (ia(1).ne.1) call abort
  a(:) = pinf
  ia = minloc (a)
  if (ia(1).ne.1) call abort
  a(1:2) = nan
  ia = minloc (a)
  if (ia(1).ne.3) call abort
  a(2) = 1.0
  ia = minloc (a)
  if (ia(1).ne.2) call abort
  a(2) = minf
  ia = minloc (a)
  if (ia(1).ne.2) call abort
  c(:) = nan
  ia = minloc (c)
  if (ia(1).ne.1) call abort
  c(:) = pinf
  ia = minloc (c)
  if (ia(1).ne.1) call abort
  c(1:2) = nan
  ia = minloc (c)
  if (ia(1).ne.3) call abort
  c(2) = 1.0
  ia = minloc (c)
  if (ia(1).ne.2) call abort
  c(2) = minf
  ia = minloc (c)
  if (ia(1).ne.2) call abort
  l = .false.
  l2(:) = .false.
  a(:) = nan
  ia = minloc (a, mask = l)
  if (ia(1).ne.0) call abort
  ia = minloc (a, mask = l2)
  if (ia(1).ne.0) call abort
  a(:) = pinf
  ia = minloc (a, mask = l)
  if (ia(1).ne.0) call abort
  ia = minloc (a, mask = l2)
  if (ia(1).ne.0) call abort
  a(1:2) = nan
  ia = minloc (a, mask = l)
  if (ia(1).ne.0) call abort
  ia = minloc (a, mask = l2)
  if (ia(1).ne.0) call abort
  a(2) = 1.0
  ia = minloc (a, mask = l)
  if (ia(1).ne.0) call abort
  ia = minloc (a, mask = l2)
  if (ia(1).ne.0) call abort
  a(2) = minf
  ia = minloc (a, mask = l)
  if (ia(1).ne.0) call abort
  ia = minloc (a, mask = l2)
  if (ia(1).ne.0) call abort
  c(:) = nan
  ia = minloc (c, mask = l)
  if (ia(1).ne.0) call abort
  ia = minloc (c, mask = l2)
  if (ia(1).ne.0) call abort
  c(:) = pinf
  ia = minloc (c, mask = l)
  if (ia(1).ne.0) call abort
  ia = minloc (c, mask = l2)
  if (ia(1).ne.0) call abort
  c(1:2) = nan
  ia = minloc (c, mask = l)
  if (ia(1).ne.0) call abort
  ia = minloc (c, mask = l2)
  if (ia(1).ne.0) call abort
  c(2) = 1.0
  ia = minloc (c, mask = l)
  if (ia(1).ne.0) call abort
  ia = minloc (c, mask = l2)
  if (ia(1).ne.0) call abort
  c(2) = minf
  ia = minloc (c, mask = l)
  if (ia(1).ne.0) call abort
  ia = minloc (c, mask = l2)
  if (ia(1).ne.0) call abort
  l = .true.
  l2(:) = .true.
  a(:) = nan
  ia = minloc (a, mask = l)
  if (ia(1).ne.1) call abort
  ia = minloc (a, mask = l2)
  if (ia(1).ne.1) call abort
  a(:) = pinf
  ia = minloc (a, mask = l)
  if (ia(1).ne.1) call abort
  ia = minloc (a, mask = l2)
  if (ia(1).ne.1) call abort
  a(1:2) = nan
  ia = minloc (a, mask = l)
  if (ia(1).ne.3) call abort
  ia = minloc (a, mask = l2)
  if (ia(1).ne.3) call abort
  a(2) = 1.0
  ia = minloc (a, mask = l)
  if (ia(1).ne.2) call abort
  ia = minloc (a, mask = l2)
  if (ia(1).ne.2) call abort
  a(2) = minf
  ia = minloc (a, mask = l)
  if (ia(1).ne.2) call abort
  ia = minloc (a, mask = l2)
  if (ia(1).ne.2) call abort
  c(:) = nan
  ia = minloc (c, mask = l)
  if (ia(1).ne.1) call abort
  ia = minloc (c, mask = l2)
  if (ia(1).ne.1) call abort
  c(:) = pinf
  ia = minloc (c, mask = l)
  if (ia(1).ne.1) call abort
  ia = minloc (c, mask = l2)
  if (ia(1).ne.1) call abort
  c(1:2) = nan
  ia = minloc (c, mask = l)
  if (ia(1).ne.3) call abort
  ia = minloc (c, mask = l2)
  if (ia(1).ne.3) call abort
  c(2) = 1.0
  ia = minloc (c, mask = l)
  if (ia(1).ne.2) call abort
  ia = minloc (c, mask = l2)
  if (ia(1).ne.2) call abort
  c(2) = minf
  ia = minloc (c, mask = l)
  if (ia(1).ne.2) call abort
  ia = minloc (c, mask = l2)
  if (ia(1).ne.2) call abort
  deallocate (c)
  allocate (c(-2:-3))
  ia = minloc (c)
  if (ia(1).ne.0) call abort
end
