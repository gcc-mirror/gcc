! { dg-do run }
! { dg-add-options ieee }
! { dg-skip-if "NaN not supported" { spu-*-* } { "*" } { "" } }
  real :: a(3,3), b(3), nan, minf, pinf, h
  logical :: l, l2
  logical :: l3(3,3), l4(3,3), l5(3,3)

  nan = 0.0
  minf = 0.0
  pinf = 0.0
  nan = 0.0/nan
  minf = -1.0/minf
  pinf = 1.0/pinf
  h = huge(h)
  l = .false.
  l2 = .true.
  l3 = .false.
  l4 = .true.
  l5 = .true.
  l5(1,1) = .false.
  l5(1,2) = .false.
  l5(2,3) = .false.
  a = reshape ((/ nan, nan, nan, pinf, pinf, pinf, pinf, minf, pinf /), (/ 3, 3 /))
  if (minval (a).ne.minf) call abort
  if (any (minloc (a).ne.(/ 2, 3 /))) call abort
  b = minval (a, dim = 1)
  if (.not.isnan(b(1))) call abort
  b(1) = 0.0
  if (any (b.ne.(/ 0.0, pinf, minf /))) call abort
  if (any (minloc (a, dim = 1).ne.(/ 1, 1, 2 /))) call abort
  b = minval (a, dim = 2)
  if (any (b.ne.(/ pinf, minf, pinf /))) call abort
  if (any (minloc (a, dim = 2).ne.(/ 2, 3, 2 /))) call abort
  if (minval (a, mask = l).ne.h) call abort
  if (any (minloc (a, mask = l).ne.(/ 0, 0 /))) call abort
  b = minval (a, dim = 1, mask = l)
  if (any (b.ne.(/ h, h, h /))) call abort
  if (any (minloc (a, dim = 1, mask = l).ne.(/ 0, 0, 0 /))) call abort
  b = minval (a, dim = 2, mask = l)
  if (any (b.ne.(/ h, h, h /))) call abort
  if (any (minloc (a, dim = 2, mask = l).ne.(/ 0, 0, 0 /))) call abort
  if (minval (a, mask = l3).ne.h) call abort
  if (any (minloc (a, mask = l3).ne.(/ 0, 0 /))) call abort
  b = minval (a, dim = 1, mask = l3)
  if (any (b.ne.(/ h, h, h /))) call abort
  if (any (minloc (a, dim = 1, mask = l3).ne.(/ 0, 0, 0 /))) call abort
  b = minval (a, dim = 2, mask = l3)
  if (any (b.ne.(/ h, h, h /))) call abort
  if (any (minloc (a, dim = 2, mask = l3).ne.(/ 0, 0, 0 /))) call abort
  if (minval (a, mask = l2).ne.minf) call abort
  if (minval (a, mask = l4).ne.minf) call abort
  if (any (minloc (a, mask = l2).ne.(/ 2, 3 /))) call abort
  if (any (minloc (a, mask = l4).ne.(/ 2, 3 /))) call abort
  b = minval (a, dim = 1, mask = l2)
  if (.not.isnan(b(1))) call abort
  b(1) = 0.0
  if (any (b.ne.(/ 0.0, pinf, minf /))) call abort
  if (any (minloc (a, dim = 1, mask = l2).ne.(/ 1, 1, 2 /))) call abort
  b = minval (a, dim = 2, mask = l2)
  if (any (b.ne.(/ pinf, minf, pinf /))) call abort
  if (any (minloc (a, dim = 2, mask = l2).ne.(/ 2, 3, 2 /))) call abort
  b = minval (a, dim = 1, mask = l4)
  if (.not.isnan(b(1))) call abort
  b(1) = 0.0
  if (any (b.ne.(/ 0.0, pinf, minf /))) call abort
  if (any (minloc (a, dim = 1, mask = l2).ne.(/ 1, 1, 2 /))) call abort
  b = minval (a, dim = 2, mask = l4)
  if (any (b.ne.(/ pinf, minf, pinf /))) call abort
  if (any (minloc (a, dim = 2, mask = l2).ne.(/ 2, 3, 2 /))) call abort
  if (minval (a, mask = l5).ne.pinf) call abort
  if (any (minloc (a, mask = l5).ne.(/ 2, 2 /))) call abort
  b = minval (a, dim = 1, mask = l5)
  if (.not.isnan(b(1))) call abort
  b(1) = 0.0
  if (any (b.ne.(/ 0.0, pinf, pinf /))) call abort
  if (any (minloc (a, dim = 1, mask = l5).ne.(/ 2, 2, 1 /))) call abort
  b = minval (a, dim = 2, mask = l5)
  if (any (b.ne.(/ pinf, pinf, pinf /))) call abort
  if (any (minloc (a, dim = 2, mask = l5).ne.(/ 3, 2, 2 /))) call abort
  a = nan
  if (.not.isnan(minval (a))) call abort
  if (minval (a, mask = l).ne.h) call abort
  if (.not.isnan(minval (a, mask = l2))) call abort
  if (minval (a, mask = l3).ne.h) call abort
  if (.not.isnan(minval (a, mask = l4))) call abort
  if (.not.isnan(minval (a, mask = l5))) call abort
  if (any (minloc (a).ne.(/ 1, 1 /))) call abort
  if (any (minloc (a, mask = l).ne.(/ 0, 0 /))) call abort
  if (any (minloc (a, mask = l2).ne.(/ 1, 1 /))) call abort
  if (any (minloc (a, mask = l3).ne.(/ 0, 0 /))) call abort
  if (any (minloc (a, mask = l4).ne.(/ 1, 1 /))) call abort
  if (any (minloc (a, mask = l5).ne.(/ 2, 1 /))) call abort
  a = pinf
  if (minval (a).ne.pinf) call abort
  if (minval (a, mask = l).ne.h) call abort
  if (minval (a, mask = l2).ne.pinf) call abort
  if (minval (a, mask = l3).ne.h) call abort
  if (minval (a, mask = l4).ne.pinf) call abort
  if (minval (a, mask = l5).ne.pinf) call abort
  if (any (minloc (a).ne.(/ 1, 1 /))) call abort
  if (any (minloc (a, mask = l).ne.(/ 0, 0 /))) call abort
  if (any (minloc (a, mask = l2).ne.(/ 1, 1 /))) call abort
  if (any (minloc (a, mask = l3).ne.(/ 0, 0 /))) call abort
  if (any (minloc (a, mask = l4).ne.(/ 1, 1 /))) call abort
  if (any (minloc (a, mask = l5).ne.(/ 2, 1 /))) call abort
  a = nan
  a(1,3) = pinf
  if (minval (a).ne.pinf) call abort
  if (minval (a, mask = l).ne.h) call abort
  if (minval (a, mask = l2).ne.pinf) call abort
  if (minval (a, mask = l3).ne.h) call abort
  if (minval (a, mask = l4).ne.pinf) call abort
  if (minval (a, mask = l5).ne.pinf) call abort
  if (any (minloc (a).ne.(/ 1, 3 /))) call abort
  if (any (minloc (a, mask = l).ne.(/ 0, 0 /))) call abort
  if (any (minloc (a, mask = l2).ne.(/ 1, 3 /))) call abort
  if (any (minloc (a, mask = l3).ne.(/ 0, 0 /))) call abort
  if (any (minloc (a, mask = l4).ne.(/ 1, 3 /))) call abort
  if (any (minloc (a, mask = l5).ne.(/ 1, 3 /))) call abort
end
