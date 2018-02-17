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
  if (minloc (a, dim = 1).ne.1) STOP 1
  if (.not.isnan(minval (a, dim = 1))) STOP 2
  a(:) = pinf
  if (minloc (a, dim = 1).ne.1) STOP 3
  if (minval (a, dim = 1).ne.pinf) STOP 4
  a(1:2) = nan
  if (minloc (a, dim = 1).ne.3) STOP 5
  if (minval (a, dim = 1).ne.pinf) STOP 6
  a(2) = 1.0
  if (minloc (a, dim = 1).ne.2) STOP 7
  if (minval (a, dim = 1).ne.1) STOP 8
  a(2) = minf
  if (minloc (a, dim = 1).ne.2) STOP 9
  if (minval (a, dim = 1).ne.minf) STOP 10
  c(:) = nan
  if (minloc (c, dim = 1).ne.1) STOP 11
  if (.not.isnan(minval (c, dim = 1))) STOP 12
  c(:) = pinf
  if (minloc (c, dim = 1).ne.1) STOP 13
  if (minval (c, dim = 1).ne.pinf) STOP 14
  c(1:2) = nan
  if (minloc (c, dim = 1).ne.3) STOP 15
  if (minval (c, dim = 1).ne.pinf) STOP 16
  c(2) = 1.0
  if (minloc (c, dim = 1).ne.2) STOP 17
  if (minval (c, dim = 1).ne.1) STOP 18
  c(2) = minf
  if (minloc (c, dim = 1).ne.2) STOP 19
  if (minval (c, dim = 1).ne.minf) STOP 20
  l = .false.
  l2(:) = .false.
  a(:) = nan
  if (minloc (a, dim = 1, mask = l).ne.0) STOP 21
  if (minval (a, dim = 1, mask = l).ne.huge(pinf)) STOP 22
  if (minloc (a, dim = 1, mask = l2).ne.0) STOP 23
  if (minval (a, dim = 1, mask = l2).ne.huge(pinf)) STOP 24
  a(:) = pinf
  if (minloc (a, dim = 1, mask = l).ne.0) STOP 25
  if (minval (a, dim = 1, mask = l).ne.huge(pinf)) STOP 26
  if (minloc (a, dim = 1, mask = l2).ne.0) STOP 27
  if (minval (a, dim = 1, mask = l2).ne.huge(pinf)) STOP 28
  a(1:2) = nan
  if (minloc (a, dim = 1, mask = l).ne.0) STOP 29
  if (minval (a, dim = 1, mask = l).ne.huge(pinf)) STOP 30
  if (minloc (a, dim = 1, mask = l2).ne.0) STOP 31
  if (minval (a, dim = 1, mask = l2).ne.huge(pinf)) STOP 32
  a(2) = 1.0
  if (minloc (a, dim = 1, mask = l).ne.0) STOP 33
  if (minval (a, dim = 1, mask = l).ne.huge(pinf)) STOP 34
  if (minloc (a, dim = 1, mask = l2).ne.0) STOP 35
  if (minval (a, dim = 1, mask = l2).ne.huge(pinf)) STOP 36
  a(2) = minf
  if (minloc (a, dim = 1, mask = l).ne.0) STOP 37
  if (minval (a, dim = 1, mask = l).ne.huge(pinf)) STOP 38
  if (minloc (a, dim = 1, mask = l2).ne.0) STOP 39
  if (minval (a, dim = 1, mask = l2).ne.huge(pinf)) STOP 40
  c(:) = nan
  if (minloc (c, dim = 1, mask = l).ne.0) STOP 41
  if (minval (c, dim = 1, mask = l).ne.huge(pinf)) STOP 42
  if (minloc (c, dim = 1, mask = l2).ne.0) STOP 43
  if (minval (c, dim = 1, mask = l2).ne.huge(pinf)) STOP 44
  c(:) = pinf
  if (minloc (c, dim = 1, mask = l).ne.0) STOP 45
  if (minval (c, dim = 1, mask = l).ne.huge(pinf)) STOP 46
  if (minloc (c, dim = 1, mask = l2).ne.0) STOP 47
  if (minval (c, dim = 1, mask = l2).ne.huge(pinf)) STOP 48
  c(1:2) = nan
  if (minloc (c, dim = 1, mask = l).ne.0) STOP 49
  if (minval (c, dim = 1, mask = l).ne.huge(pinf)) STOP 50
  if (minloc (c, dim = 1, mask = l2).ne.0) STOP 51
  if (minval (c, dim = 1, mask = l2).ne.huge(pinf)) STOP 52
  c(2) = 1.0
  if (minloc (c, dim = 1, mask = l).ne.0) STOP 53
  if (minval (c, dim = 1, mask = l).ne.huge(pinf)) STOP 54
  if (minloc (c, dim = 1, mask = l2).ne.0) STOP 55
  if (minval (c, dim = 1, mask = l2).ne.huge(pinf)) STOP 56
  c(2) = minf
  if (minloc (c, dim = 1, mask = l).ne.0) STOP 57
  if (minval (c, dim = 1, mask = l).ne.huge(pinf)) STOP 58
  if (minloc (c, dim = 1, mask = l2).ne.0) STOP 59
  if (minval (c, dim = 1, mask = l2).ne.huge(pinf)) STOP 60
  l = .true.
  l2(:) = .true.
  a(:) = nan
  if (minloc (a, dim = 1, mask = l).ne.1) STOP 61
  if (.not.isnan(minval (a, dim = 1, mask = l))) STOP 62
  if (minloc (a, dim = 1, mask = l2).ne.1) STOP 63
  if (.not.isnan(minval (a, dim = 1, mask = l2))) STOP 64
  a(:) = pinf
  if (minloc (a, dim = 1, mask = l).ne.1) STOP 65
  if (minval (a, dim = 1, mask = l).ne.pinf) STOP 66
  if (minloc (a, dim = 1, mask = l2).ne.1) STOP 67
  if (minval (a, dim = 1, mask = l2).ne.pinf) STOP 68
  a(1:2) = nan
  if (minloc (a, dim = 1, mask = l).ne.3) STOP 69
  if (minval (a, dim = 1, mask = l).ne.pinf) STOP 70
  if (minloc (a, dim = 1, mask = l2).ne.3) STOP 71
  if (minval (a, dim = 1, mask = l2).ne.pinf) STOP 72
  a(2) = 1.0
  if (minloc (a, dim = 1, mask = l).ne.2) STOP 73
  if (minval (a, dim = 1, mask = l).ne.1) STOP 74
  if (minloc (a, dim = 1, mask = l2).ne.2) STOP 75
  if (minval (a, dim = 1, mask = l2).ne.1) STOP 76
  a(2) = minf
  if (minloc (a, dim = 1, mask = l).ne.2) STOP 77
  if (minval (a, dim = 1, mask = l).ne.minf) STOP 78
  if (minloc (a, dim = 1, mask = l2).ne.2) STOP 79
  if (minval (a, dim = 1, mask = l2).ne.minf) STOP 80
  c(:) = nan
  if (minloc (c, dim = 1, mask = l).ne.1) STOP 81
  if (.not.isnan(minval (c, dim = 1, mask = l))) STOP 82
  if (minloc (c, dim = 1, mask = l2).ne.1) STOP 83
  if (.not.isnan(minval (c, dim = 1, mask = l2))) STOP 84
  c(:) = pinf
  if (minloc (c, dim = 1, mask = l).ne.1) STOP 85
  if (minval (c, dim = 1, mask = l).ne.pinf) STOP 86
  if (minloc (c, dim = 1, mask = l2).ne.1) STOP 87
  if (minval (c, dim = 1, mask = l2).ne.pinf) STOP 88
  c(1:2) = nan
  if (minloc (c, dim = 1, mask = l).ne.3) STOP 89
  if (minval (c, dim = 1, mask = l).ne.pinf) STOP 90
  if (minloc (c, dim = 1, mask = l2).ne.3) STOP 91
  if (minval (c, dim = 1, mask = l2).ne.pinf) STOP 92
  c(2) = 1.0
  if (minloc (c, dim = 1, mask = l).ne.2) STOP 93
  if (minval (c, dim = 1, mask = l).ne.1) STOP 94
  if (minloc (c, dim = 1, mask = l2).ne.2) STOP 95
  if (minval (c, dim = 1, mask = l2).ne.1) STOP 96
  c(2) = minf
  if (minloc (c, dim = 1, mask = l).ne.2) STOP 97
  if (minval (c, dim = 1, mask = l).ne.minf) STOP 98
  if (minloc (c, dim = 1, mask = l2).ne.2) STOP 99
  if (minval (c, dim = 1, mask = l2).ne.minf) STOP 100
  deallocate (c)
  allocate (c(-2:-3))
  if (minloc (c, dim = 1).ne.0) STOP 101
  if (minval (c, dim = 1).ne.huge(pinf)) STOP 102
end
