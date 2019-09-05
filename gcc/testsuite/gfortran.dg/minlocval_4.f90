! { dg-do run }
! { dg-add-options ieee }
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
  if (minval (a).ne.minf) STOP 1
  if (any (minloc (a).ne.(/ 2, 3 /))) STOP 2
  b = minval (a, dim = 1)
  if (.not.isnan(b(1))) STOP 3
  b(1) = 0.0
  if (any (b.ne.(/ 0.0, pinf, minf /))) STOP 4
  if (any (minloc (a, dim = 1).ne.(/ 1, 1, 2 /))) STOP 5
  b = minval (a, dim = 2)
  if (any (b.ne.(/ pinf, minf, pinf /))) STOP 6
  if (any (minloc (a, dim = 2).ne.(/ 2, 3, 2 /))) STOP 7
  if (minval (a, mask = l).ne.h) STOP 8
  if (any (minloc (a, mask = l).ne.(/ 0, 0 /))) STOP 9
  b = minval (a, dim = 1, mask = l)
  if (any (b.ne.(/ h, h, h /))) STOP 10
  if (any (minloc (a, dim = 1, mask = l).ne.(/ 0, 0, 0 /))) STOP 11
  b = minval (a, dim = 2, mask = l)
  if (any (b.ne.(/ h, h, h /))) STOP 12
  if (any (minloc (a, dim = 2, mask = l).ne.(/ 0, 0, 0 /))) STOP 13
  if (minval (a, mask = l3).ne.h) STOP 14
  if (any (minloc (a, mask = l3).ne.(/ 0, 0 /))) STOP 15
  b = minval (a, dim = 1, mask = l3)
  if (any (b.ne.(/ h, h, h /))) STOP 16
  if (any (minloc (a, dim = 1, mask = l3).ne.(/ 0, 0, 0 /))) STOP 17
  b = minval (a, dim = 2, mask = l3)
  if (any (b.ne.(/ h, h, h /))) STOP 18
  if (any (minloc (a, dim = 2, mask = l3).ne.(/ 0, 0, 0 /))) STOP 19
  if (minval (a, mask = l2).ne.minf) STOP 20
  if (minval (a, mask = l4).ne.minf) STOP 21
  if (any (minloc (a, mask = l2).ne.(/ 2, 3 /))) STOP 22
  if (any (minloc (a, mask = l4).ne.(/ 2, 3 /))) STOP 23
  b = minval (a, dim = 1, mask = l2)
  if (.not.isnan(b(1))) STOP 24
  b(1) = 0.0
  if (any (b.ne.(/ 0.0, pinf, minf /))) STOP 25
  if (any (minloc (a, dim = 1, mask = l2).ne.(/ 1, 1, 2 /))) STOP 26
  b = minval (a, dim = 2, mask = l2)
  if (any (b.ne.(/ pinf, minf, pinf /))) STOP 27
  if (any (minloc (a, dim = 2, mask = l2).ne.(/ 2, 3, 2 /))) STOP 28
  b = minval (a, dim = 1, mask = l4)
  if (.not.isnan(b(1))) STOP 29
  b(1) = 0.0
  if (any (b.ne.(/ 0.0, pinf, minf /))) STOP 30
  if (any (minloc (a, dim = 1, mask = l2).ne.(/ 1, 1, 2 /))) STOP 31
  b = minval (a, dim = 2, mask = l4)
  if (any (b.ne.(/ pinf, minf, pinf /))) STOP 32
  if (any (minloc (a, dim = 2, mask = l2).ne.(/ 2, 3, 2 /))) STOP 33
  if (minval (a, mask = l5).ne.pinf) STOP 34
  if (any (minloc (a, mask = l5).ne.(/ 2, 2 /))) STOP 35
  b = minval (a, dim = 1, mask = l5)
  if (.not.isnan(b(1))) STOP 36
  b(1) = 0.0
  if (any (b.ne.(/ 0.0, pinf, pinf /))) STOP 37
  if (any (minloc (a, dim = 1, mask = l5).ne.(/ 2, 2, 1 /))) STOP 38
  b = minval (a, dim = 2, mask = l5)
  if (any (b.ne.(/ pinf, pinf, pinf /))) STOP 39
  if (any (minloc (a, dim = 2, mask = l5).ne.(/ 3, 2, 2 /))) STOP 40
  a = nan
  if (.not.isnan(minval (a))) STOP 41
  if (minval (a, mask = l).ne.h) STOP 42
  if (.not.isnan(minval (a, mask = l2))) STOP 43
  if (minval (a, mask = l3).ne.h) STOP 44
  if (.not.isnan(minval (a, mask = l4))) STOP 45
  if (.not.isnan(minval (a, mask = l5))) STOP 46
  if (any (minloc (a).ne.(/ 1, 1 /))) STOP 47
  if (any (minloc (a, mask = l).ne.(/ 0, 0 /))) STOP 48
  if (any (minloc (a, mask = l2).ne.(/ 1, 1 /))) STOP 49
  if (any (minloc (a, mask = l3).ne.(/ 0, 0 /))) STOP 50
  if (any (minloc (a, mask = l4).ne.(/ 1, 1 /))) STOP 51
  if (any (minloc (a, mask = l5).ne.(/ 2, 1 /))) STOP 52
  a = pinf
  if (minval (a).ne.pinf) STOP 53
  if (minval (a, mask = l).ne.h) STOP 54
  if (minval (a, mask = l2).ne.pinf) STOP 55
  if (minval (a, mask = l3).ne.h) STOP 56
  if (minval (a, mask = l4).ne.pinf) STOP 57
  if (minval (a, mask = l5).ne.pinf) STOP 58
  if (any (minloc (a).ne.(/ 1, 1 /))) STOP 59
  if (any (minloc (a, mask = l).ne.(/ 0, 0 /))) STOP 60
  if (any (minloc (a, mask = l2).ne.(/ 1, 1 /))) STOP 61
  if (any (minloc (a, mask = l3).ne.(/ 0, 0 /))) STOP 62
  if (any (minloc (a, mask = l4).ne.(/ 1, 1 /))) STOP 63
  if (any (minloc (a, mask = l5).ne.(/ 2, 1 /))) STOP 64
  a = nan
  a(1,3) = pinf
  if (minval (a).ne.pinf) STOP 65
  if (minval (a, mask = l).ne.h) STOP 66
  if (minval (a, mask = l2).ne.pinf) STOP 67
  if (minval (a, mask = l3).ne.h) STOP 68
  if (minval (a, mask = l4).ne.pinf) STOP 69
  if (minval (a, mask = l5).ne.pinf) STOP 70
  if (any (minloc (a).ne.(/ 1, 3 /))) STOP 71
  if (any (minloc (a, mask = l).ne.(/ 0, 0 /))) STOP 72
  if (any (minloc (a, mask = l2).ne.(/ 1, 3 /))) STOP 73
  if (any (minloc (a, mask = l3).ne.(/ 0, 0 /))) STOP 74
  if (any (minloc (a, mask = l4).ne.(/ 1, 3 /))) STOP 75
  if (any (minloc (a, mask = l5).ne.(/ 1, 3 /))) STOP 76
end
