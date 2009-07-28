! { dg-do run }
! { dg-options "-mieee" { target alpha*-*-* sh*-*-* } } 
! { dg-skip-if "NaN not supported" { spu-*-* } { "*" } { "" } }
  real :: a(3), nan, minf, pinf
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
  if (maxloc (a, dim = 1).ne.1) call abort
  if (.not.isnan(maxval (a, dim = 1))) call abort
  a(:) = minf
  if (maxloc (a, dim = 1).ne.1) call abort
  if (maxval (a, dim = 1).ne.minf) call abort
  a(1:2) = nan
  if (maxloc (a, dim = 1).ne.3) call abort
  if (maxval (a, dim = 1).ne.minf) call abort
  a(2) = 1.0
  if (maxloc (a, dim = 1).ne.2) call abort
  if (maxval (a, dim = 1).ne.1) call abort
  a(2) = pinf
  if (maxloc (a, dim = 1).ne.2) call abort
  if (maxval (a, dim = 1).ne.pinf) call abort
  c(:) = nan
  if (maxloc (c, dim = 1).ne.1) call abort
  if (.not.isnan(maxval (c, dim = 1))) call abort
  c(:) = minf
  if (maxloc (c, dim = 1).ne.1) call abort
  if (maxval (c, dim = 1).ne.minf) call abort
  c(1:2) = nan
  if (maxloc (c, dim = 1).ne.3) call abort
  if (maxval (c, dim = 1).ne.minf) call abort
  c(2) = 1.0
  if (maxloc (c, dim = 1).ne.2) call abort
  if (maxval (c, dim = 1).ne.1) call abort
  c(2) = pinf
  if (maxloc (c, dim = 1).ne.2) call abort
  if (maxval (c, dim = 1).ne.pinf) call abort
  l = .false.
  l2(:) = .false.
  a(:) = nan
  if (maxloc (a, dim = 1, mask = l).ne.0) call abort
  if (maxval (a, dim = 1, mask = l).ne.-huge(minf)) call abort
  if (maxloc (a, dim = 1, mask = l2).ne.0) call abort
  if (maxval (a, dim = 1, mask = l2).ne.-huge(minf)) call abort
  a(:) = minf
  if (maxloc (a, dim = 1, mask = l).ne.0) call abort
  if (maxval (a, dim = 1, mask = l).ne.-huge(minf)) call abort
  if (maxloc (a, dim = 1, mask = l2).ne.0) call abort
  if (maxval (a, dim = 1, mask = l2).ne.-huge(minf)) call abort
  a(1:2) = nan
  if (maxloc (a, dim = 1, mask = l).ne.0) call abort
  if (maxval (a, dim = 1, mask = l).ne.-huge(minf)) call abort
  if (maxloc (a, dim = 1, mask = l2).ne.0) call abort
  if (maxval (a, dim = 1, mask = l2).ne.-huge(minf)) call abort
  a(2) = 1.0
  if (maxloc (a, dim = 1, mask = l).ne.0) call abort
  if (maxval (a, dim = 1, mask = l).ne.-huge(minf)) call abort
  if (maxloc (a, dim = 1, mask = l2).ne.0) call abort
  if (maxval (a, dim = 1, mask = l2).ne.-huge(minf)) call abort
  a(2) = pinf
  if (maxloc (a, dim = 1, mask = l).ne.0) call abort
  if (maxval (a, dim = 1, mask = l).ne.-huge(minf)) call abort
  if (maxloc (a, dim = 1, mask = l2).ne.0) call abort
  if (maxval (a, dim = 1, mask = l2).ne.-huge(minf)) call abort
  c(:) = nan
  if (maxloc (c, dim = 1, mask = l).ne.0) call abort
  if (maxval (c, dim = 1, mask = l).ne.-huge(minf)) call abort
  if (maxloc (c, dim = 1, mask = l2).ne.0) call abort
  if (maxval (c, dim = 1, mask = l2).ne.-huge(minf)) call abort
  c(:) = minf
  if (maxloc (c, dim = 1, mask = l).ne.0) call abort
  if (maxval (c, dim = 1, mask = l).ne.-huge(minf)) call abort
  if (maxloc (c, dim = 1, mask = l2).ne.0) call abort
  if (maxval (c, dim = 1, mask = l2).ne.-huge(minf)) call abort
  c(1:2) = nan
  if (maxloc (c, dim = 1, mask = l).ne.0) call abort
  if (maxval (c, dim = 1, mask = l).ne.-huge(minf)) call abort
  if (maxloc (c, dim = 1, mask = l2).ne.0) call abort
  if (maxval (c, dim = 1, mask = l2).ne.-huge(minf)) call abort
  c(2) = 1.0
  if (maxloc (c, dim = 1, mask = l).ne.0) call abort
  if (maxval (c, dim = 1, mask = l).ne.-huge(minf)) call abort
  if (maxloc (c, dim = 1, mask = l2).ne.0) call abort
  if (maxval (c, dim = 1, mask = l2).ne.-huge(minf)) call abort
  c(2) = pinf
  if (maxloc (c, dim = 1, mask = l).ne.0) call abort
  if (maxval (c, dim = 1, mask = l).ne.-huge(minf)) call abort
  if (maxloc (c, dim = 1, mask = l2).ne.0) call abort
  if (maxval (c, dim = 1, mask = l2).ne.-huge(minf)) call abort
  l = .true.
  l2(:) = .true.
  a(:) = nan
  if (maxloc (a, dim = 1, mask = l).ne.1) call abort
  if (.not.isnan(maxval (a, dim = 1, mask = l))) call abort
  if (maxloc (a, dim = 1, mask = l2).ne.1) call abort
  if (.not.isnan(maxval (a, dim = 1, mask = l2))) call abort
  a(:) = minf
  if (maxloc (a, dim = 1, mask = l).ne.1) call abort
  if (maxval (a, dim = 1, mask = l).ne.minf) call abort
  if (maxloc (a, dim = 1, mask = l2).ne.1) call abort
  if (maxval (a, dim = 1, mask = l2).ne.minf) call abort
  a(1:2) = nan
  if (maxloc (a, dim = 1, mask = l).ne.3) call abort
  if (maxval (a, dim = 1, mask = l).ne.minf) call abort
  if (maxloc (a, dim = 1, mask = l2).ne.3) call abort
  if (maxval (a, dim = 1, mask = l2).ne.minf) call abort
  a(2) = 1.0
  if (maxloc (a, dim = 1, mask = l).ne.2) call abort
  if (maxval (a, dim = 1, mask = l).ne.1) call abort
  if (maxloc (a, dim = 1, mask = l2).ne.2) call abort
  if (maxval (a, dim = 1, mask = l2).ne.1) call abort
  a(2) = pinf
  if (maxloc (a, dim = 1, mask = l).ne.2) call abort
  if (maxval (a, dim = 1, mask = l).ne.pinf) call abort
  if (maxloc (a, dim = 1, mask = l2).ne.2) call abort
  if (maxval (a, dim = 1, mask = l2).ne.pinf) call abort
  c(:) = nan
  if (maxloc (c, dim = 1, mask = l).ne.1) call abort
  if (.not.isnan(maxval (c, dim = 1, mask = l))) call abort
  if (maxloc (c, dim = 1, mask = l2).ne.1) call abort
  if (.not.isnan(maxval (c, dim = 1, mask = l2))) call abort
  c(:) = minf
  if (maxloc (c, dim = 1, mask = l).ne.1) call abort
  if (maxval (c, dim = 1, mask = l).ne.minf) call abort
  if (maxloc (c, dim = 1, mask = l2).ne.1) call abort
  if (maxval (c, dim = 1, mask = l2).ne.minf) call abort
  c(1:2) = nan
  if (maxloc (c, dim = 1, mask = l).ne.3) call abort
  if (maxval (c, dim = 1, mask = l).ne.minf) call abort
  if (maxloc (c, dim = 1, mask = l2).ne.3) call abort
  if (maxval (c, dim = 1, mask = l2).ne.minf) call abort
  c(2) = 1.0
  if (maxloc (c, dim = 1, mask = l).ne.2) call abort
  if (maxval (c, dim = 1, mask = l).ne.1) call abort
  if (maxloc (c, dim = 1, mask = l2).ne.2) call abort
  if (maxval (c, dim = 1, mask = l2).ne.1) call abort
  c(2) = pinf
  if (maxloc (c, dim = 1, mask = l).ne.2) call abort
  if (maxval (c, dim = 1, mask = l).ne.pinf) call abort
  if (maxloc (c, dim = 1, mask = l2).ne.2) call abort
  if (maxval (c, dim = 1, mask = l2).ne.pinf) call abort
  deallocate (c)
  allocate (c(-2:-3))
  if (maxloc (c, dim = 1).ne.0) call abort
  if (maxval (c, dim = 1).ne.-huge(minf)) call abort
end
