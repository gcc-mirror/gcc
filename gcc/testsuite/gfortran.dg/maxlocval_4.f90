! { dg-do run }
! { dg-add-options ieee }
! { dg-skip-if "NaN not supported" { spu-*-* } }
  real :: a(3,3), b(3), nan, minf, pinf, h
  logical :: l, l2
  logical :: l3(3,3), l4(3,3), l5(3,3)

  nan = 0.0
  minf = 0.0
  pinf = 0.0
  nan = 0.0/nan
  minf = -1.0/minf
  pinf = 1.0/pinf
  h = -huge(h)
  l = .false.
  l2 = .true.
  l3 = .false.
  l4 = .true.
  l5 = .true.
  l5(1,1) = .false.
  l5(1,2) = .false.
  l5(2,3) = .false.
  a = reshape ((/ nan, nan, nan, minf, minf, minf, minf, pinf, minf /), (/ 3, 3 /))
  if (maxval (a).ne.pinf) STOP 1
  if (any (maxloc (a).ne.(/ 2, 3 /))) STOP 2
  b = maxval (a, dim = 1)
  if (.not.isnan(b(1))) STOP 3
  b(1) = 0.0
  if (any (b.ne.(/ 0.0, minf, pinf /))) STOP 4
  if (any (maxloc (a, dim = 1).ne.(/ 1, 1, 2 /))) STOP 5
  b = maxval (a, dim = 2)
  if (any (b.ne.(/ minf, pinf, minf /))) STOP 6
  if (any (maxloc (a, dim = 2).ne.(/ 2, 3, 2 /))) STOP 7
  if (maxval (a, mask = l).ne.h) STOP 8
  if (any (maxloc (a, mask = l).ne.(/ 0, 0 /))) STOP 9
  b = maxval (a, dim = 1, mask = l)
  if (any (b.ne.(/ h, h, h /))) STOP 10
  if (any (maxloc (a, dim = 1, mask = l).ne.(/ 0, 0, 0 /))) STOP 11
  b = maxval (a, dim = 2, mask = l)
  if (any (b.ne.(/ h, h, h /))) STOP 12
  if (any (maxloc (a, dim = 2, mask = l).ne.(/ 0, 0, 0 /))) STOP 13
  if (maxval (a, mask = l3).ne.h) STOP 14
  if (any (maxloc (a, mask = l3).ne.(/ 0, 0 /))) STOP 15
  b = maxval (a, dim = 1, mask = l3)
  if (any (b.ne.(/ h, h, h /))) STOP 16
  if (any (maxloc (a, dim = 1, mask = l3).ne.(/ 0, 0, 0 /))) STOP 17
  b = maxval (a, dim = 2, mask = l3)
  if (any (b.ne.(/ h, h, h /))) STOP 18
  if (any (maxloc (a, dim = 2, mask = l3).ne.(/ 0, 0, 0 /))) STOP 19
  if (maxval (a, mask = l2).ne.pinf) STOP 20
  if (maxval (a, mask = l4).ne.pinf) STOP 21
  if (any (maxloc (a, mask = l2).ne.(/ 2, 3 /))) STOP 22
  if (any (maxloc (a, mask = l4).ne.(/ 2, 3 /))) STOP 23
  b = maxval (a, dim = 1, mask = l2)
  if (.not.isnan(b(1))) STOP 24
  b(1) = 0.0
  if (any (b.ne.(/ 0.0, minf, pinf /))) STOP 25
  if (any (maxloc (a, dim = 1, mask = l2).ne.(/ 1, 1, 2 /))) STOP 26
  b = maxval (a, dim = 2, mask = l2)
  if (any (b.ne.(/ minf, pinf, minf /))) STOP 27
  if (any (maxloc (a, dim = 2, mask = l2).ne.(/ 2, 3, 2 /))) STOP 28
  b = maxval (a, dim = 1, mask = l4)
  if (.not.isnan(b(1))) STOP 29
  b(1) = 0.0
  if (any (b.ne.(/ 0.0, minf, pinf /))) STOP 30
  if (any (maxloc (a, dim = 1, mask = l2).ne.(/ 1, 1, 2 /))) STOP 31
  b = maxval (a, dim = 2, mask = l4)
  if (any (b.ne.(/ minf, pinf, minf /))) STOP 32
  if (any (maxloc (a, dim = 2, mask = l2).ne.(/ 2, 3, 2 /))) STOP 33
  if (maxval (a, mask = l5).ne.minf) STOP 34
  if (any (maxloc (a, mask = l5).ne.(/ 2, 2 /))) STOP 35
  b = maxval (a, dim = 1, mask = l5)
  if (.not.isnan(b(1))) STOP 36
  b(1) = 0.0
  if (any (b.ne.(/ 0.0, minf, minf /))) STOP 37
  if (any (maxloc (a, dim = 1, mask = l5).ne.(/ 2, 2, 1 /))) STOP 38
  b = maxval (a, dim = 2, mask = l5)
  if (any (b.ne.(/ minf, minf, minf /))) STOP 39
  if (any (maxloc (a, dim = 2, mask = l5).ne.(/ 3, 2, 2 /))) STOP 40
  a = nan
  if (.not.isnan(maxval (a))) STOP 41
  if (maxval (a, mask = l).ne.h) STOP 42
  if (.not.isnan(maxval (a, mask = l2))) STOP 43
  if (maxval (a, mask = l3).ne.h) STOP 44
  if (.not.isnan(maxval (a, mask = l4))) STOP 45
  if (.not.isnan(maxval (a, mask = l5))) STOP 46
  if (any (maxloc (a).ne.(/ 1, 1 /))) STOP 47
  if (any (maxloc (a, mask = l).ne.(/ 0, 0 /))) STOP 48
  if (any (maxloc (a, mask = l2).ne.(/ 1, 1 /))) STOP 49
  if (any (maxloc (a, mask = l3).ne.(/ 0, 0 /))) STOP 50
  if (any (maxloc (a, mask = l4).ne.(/ 1, 1 /))) STOP 51
  if (any (maxloc (a, mask = l5).ne.(/ 2, 1 /))) STOP 52
  a = minf
  if (maxval (a).ne.minf) STOP 53
  if (maxval (a, mask = l).ne.h) STOP 54
  if (maxval (a, mask = l2).ne.minf) STOP 55
  if (maxval (a, mask = l3).ne.h) STOP 56
  if (maxval (a, mask = l4).ne.minf) STOP 57
  if (maxval (a, mask = l5).ne.minf) STOP 58
  if (any (maxloc (a).ne.(/ 1, 1 /))) STOP 59
  if (any (maxloc (a, mask = l).ne.(/ 0, 0 /))) STOP 60
  if (any (maxloc (a, mask = l2).ne.(/ 1, 1 /))) STOP 61
  if (any (maxloc (a, mask = l3).ne.(/ 0, 0 /))) STOP 62
  if (any (maxloc (a, mask = l4).ne.(/ 1, 1 /))) STOP 63
  if (any (maxloc (a, mask = l5).ne.(/ 2, 1 /))) STOP 64
  a = nan
  a(1,3) = minf
  if (maxval (a).ne.minf) STOP 65
  if (maxval (a, mask = l).ne.h) STOP 66
  if (maxval (a, mask = l2).ne.minf) STOP 67
  if (maxval (a, mask = l3).ne.h) STOP 68
  if (maxval (a, mask = l4).ne.minf) STOP 69
  if (maxval (a, mask = l5).ne.minf) STOP 70
  if (any (maxloc (a).ne.(/ 1, 3 /))) STOP 71
  if (any (maxloc (a, mask = l).ne.(/ 0, 0 /))) STOP 72
  if (any (maxloc (a, mask = l2).ne.(/ 1, 3 /))) STOP 73
  if (any (maxloc (a, mask = l3).ne.(/ 0, 0 /))) STOP 74
  if (any (maxloc (a, mask = l4).ne.(/ 1, 3 /))) STOP 75
  if (any (maxloc (a, mask = l5).ne.(/ 1, 3 /))) STOP 76
end
