! { dg-do run }
! { dg-add-options ieee }
! { dg-skip-if "NaN not supported" { spu-*-* } }
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
  if (maxloc (a, dim = 1).ne.1) STOP 1
  if (.not.isnan(maxval (a, dim = 1))) STOP 2
  a(:) = minf
  if (maxloc (a, dim = 1).ne.1) STOP 3
  if (maxval (a, dim = 1).ne.minf) STOP 4
  a(1:2) = nan
  if (maxloc (a, dim = 1).ne.3) STOP 5
  if (maxval (a, dim = 1).ne.minf) STOP 6
  a(2) = 1.0
  if (maxloc (a, dim = 1).ne.2) STOP 7
  if (maxval (a, dim = 1).ne.1) STOP 8
  a(2) = pinf
  if (maxloc (a, dim = 1).ne.2) STOP 9
  if (maxval (a, dim = 1).ne.pinf) STOP 10
  c(:) = nan
  if (maxloc (c, dim = 1).ne.1) STOP 11
  if (.not.isnan(maxval (c, dim = 1))) STOP 12
  c(:) = minf
  if (maxloc (c, dim = 1).ne.1) STOP 13
  if (maxval (c, dim = 1).ne.minf) STOP 14
  c(1:2) = nan
  if (maxloc (c, dim = 1).ne.3) STOP 15
  if (maxval (c, dim = 1).ne.minf) STOP 16
  c(2) = 1.0
  if (maxloc (c, dim = 1).ne.2) STOP 17
  if (maxval (c, dim = 1).ne.1) STOP 18
  c(2) = pinf
  if (maxloc (c, dim = 1).ne.2) STOP 19
  if (maxval (c, dim = 1).ne.pinf) STOP 20
  l = .false.
  l2(:) = .false.
  a(:) = nan
  if (maxloc (a, dim = 1, mask = l).ne.0) STOP 21
  if (maxval (a, dim = 1, mask = l).ne.-huge(minf)) STOP 22
  if (maxloc (a, dim = 1, mask = l2).ne.0) STOP 23
  if (maxval (a, dim = 1, mask = l2).ne.-huge(minf)) STOP 24
  a(:) = minf
  if (maxloc (a, dim = 1, mask = l).ne.0) STOP 25
  if (maxval (a, dim = 1, mask = l).ne.-huge(minf)) STOP 26
  if (maxloc (a, dim = 1, mask = l2).ne.0) STOP 27
  if (maxval (a, dim = 1, mask = l2).ne.-huge(minf)) STOP 28
  a(1:2) = nan
  if (maxloc (a, dim = 1, mask = l).ne.0) STOP 29
  if (maxval (a, dim = 1, mask = l).ne.-huge(minf)) STOP 30
  if (maxloc (a, dim = 1, mask = l2).ne.0) STOP 31
  if (maxval (a, dim = 1, mask = l2).ne.-huge(minf)) STOP 32
  a(2) = 1.0
  if (maxloc (a, dim = 1, mask = l).ne.0) STOP 33
  if (maxval (a, dim = 1, mask = l).ne.-huge(minf)) STOP 34
  if (maxloc (a, dim = 1, mask = l2).ne.0) STOP 35
  if (maxval (a, dim = 1, mask = l2).ne.-huge(minf)) STOP 36
  a(2) = pinf
  if (maxloc (a, dim = 1, mask = l).ne.0) STOP 37
  if (maxval (a, dim = 1, mask = l).ne.-huge(minf)) STOP 38
  if (maxloc (a, dim = 1, mask = l2).ne.0) STOP 39
  if (maxval (a, dim = 1, mask = l2).ne.-huge(minf)) STOP 40
  c(:) = nan
  if (maxloc (c, dim = 1, mask = l).ne.0) STOP 41
  if (maxval (c, dim = 1, mask = l).ne.-huge(minf)) STOP 42
  if (maxloc (c, dim = 1, mask = l2).ne.0) STOP 43
  if (maxval (c, dim = 1, mask = l2).ne.-huge(minf)) STOP 44
  c(:) = minf
  if (maxloc (c, dim = 1, mask = l).ne.0) STOP 45
  if (maxval (c, dim = 1, mask = l).ne.-huge(minf)) STOP 46
  if (maxloc (c, dim = 1, mask = l2).ne.0) STOP 47
  if (maxval (c, dim = 1, mask = l2).ne.-huge(minf)) STOP 48
  c(1:2) = nan
  if (maxloc (c, dim = 1, mask = l).ne.0) STOP 49
  if (maxval (c, dim = 1, mask = l).ne.-huge(minf)) STOP 50
  if (maxloc (c, dim = 1, mask = l2).ne.0) STOP 51
  if (maxval (c, dim = 1, mask = l2).ne.-huge(minf)) STOP 52
  c(2) = 1.0
  if (maxloc (c, dim = 1, mask = l).ne.0) STOP 53
  if (maxval (c, dim = 1, mask = l).ne.-huge(minf)) STOP 54
  if (maxloc (c, dim = 1, mask = l2).ne.0) STOP 55
  if (maxval (c, dim = 1, mask = l2).ne.-huge(minf)) STOP 56
  c(2) = pinf
  if (maxloc (c, dim = 1, mask = l).ne.0) STOP 57
  if (maxval (c, dim = 1, mask = l).ne.-huge(minf)) STOP 58
  if (maxloc (c, dim = 1, mask = l2).ne.0) STOP 59
  if (maxval (c, dim = 1, mask = l2).ne.-huge(minf)) STOP 60
  l = .true.
  l2(:) = .true.
  a(:) = nan
  if (maxloc (a, dim = 1, mask = l).ne.1) STOP 61
  if (.not.isnan(maxval (a, dim = 1, mask = l))) STOP 62
  if (maxloc (a, dim = 1, mask = l2).ne.1) STOP 63
  if (.not.isnan(maxval (a, dim = 1, mask = l2))) STOP 64
  a(:) = minf
  if (maxloc (a, dim = 1, mask = l).ne.1) STOP 65
  if (maxval (a, dim = 1, mask = l).ne.minf) STOP 66
  if (maxloc (a, dim = 1, mask = l2).ne.1) STOP 67
  if (maxval (a, dim = 1, mask = l2).ne.minf) STOP 68
  a(1:2) = nan
  if (maxloc (a, dim = 1, mask = l).ne.3) STOP 69
  if (maxval (a, dim = 1, mask = l).ne.minf) STOP 70
  if (maxloc (a, dim = 1, mask = l2).ne.3) STOP 71
  if (maxval (a, dim = 1, mask = l2).ne.minf) STOP 72
  a(2) = 1.0
  if (maxloc (a, dim = 1, mask = l).ne.2) STOP 73
  if (maxval (a, dim = 1, mask = l).ne.1) STOP 74
  if (maxloc (a, dim = 1, mask = l2).ne.2) STOP 75
  if (maxval (a, dim = 1, mask = l2).ne.1) STOP 76
  a(2) = pinf
  if (maxloc (a, dim = 1, mask = l).ne.2) STOP 77
  if (maxval (a, dim = 1, mask = l).ne.pinf) STOP 78
  if (maxloc (a, dim = 1, mask = l2).ne.2) STOP 79
  if (maxval (a, dim = 1, mask = l2).ne.pinf) STOP 80
  c(:) = nan
  if (maxloc (c, dim = 1, mask = l).ne.1) STOP 81
  if (.not.isnan(maxval (c, dim = 1, mask = l))) STOP 82
  if (maxloc (c, dim = 1, mask = l2).ne.1) STOP 83
  if (.not.isnan(maxval (c, dim = 1, mask = l2))) STOP 84
  c(:) = minf
  if (maxloc (c, dim = 1, mask = l).ne.1) STOP 85
  if (maxval (c, dim = 1, mask = l).ne.minf) STOP 86
  if (maxloc (c, dim = 1, mask = l2).ne.1) STOP 87
  if (maxval (c, dim = 1, mask = l2).ne.minf) STOP 88
  c(1:2) = nan
  if (maxloc (c, dim = 1, mask = l).ne.3) STOP 89
  if (maxval (c, dim = 1, mask = l).ne.minf) STOP 90
  if (maxloc (c, dim = 1, mask = l2).ne.3) STOP 91
  if (maxval (c, dim = 1, mask = l2).ne.minf) STOP 92
  c(2) = 1.0
  if (maxloc (c, dim = 1, mask = l).ne.2) STOP 93
  if (maxval (c, dim = 1, mask = l).ne.1) STOP 94
  if (maxloc (c, dim = 1, mask = l2).ne.2) STOP 95
  if (maxval (c, dim = 1, mask = l2).ne.1) STOP 96
  c(2) = pinf
  if (maxloc (c, dim = 1, mask = l).ne.2) STOP 97
  if (maxval (c, dim = 1, mask = l).ne.pinf) STOP 98
  if (maxloc (c, dim = 1, mask = l2).ne.2) STOP 99
  if (maxval (c, dim = 1, mask = l2).ne.pinf) STOP 100
  deallocate (c)
  allocate (c(-2:-3))
  if (maxloc (c, dim = 1).ne.0) STOP 101
  if (maxval (c, dim = 1).ne.-huge(minf)) STOP 102
end
