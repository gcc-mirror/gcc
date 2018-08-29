! { dg-do run }
  integer :: a(3), h
  integer, allocatable :: c(:)
  logical :: l
  logical :: l2(3)

  h = -huge(h)
  h = h - 1
  allocate (c(3))
  a(:) = 5
  if (minloc (a, dim = 1).ne.1) STOP 1
  if (minval (a, dim = 1).ne.5) STOP 2
  a(2) = h
  if (minloc (a, dim = 1).ne.2) STOP 3
  if (minval (a, dim = 1).ne.h) STOP 4
  a(:) = huge(h)
  if (minloc (a, dim = 1).ne.1) STOP 5
  if (minval (a, dim = 1).ne.huge(h)) STOP 6
  a(3) = huge(h) - 1
  if (minloc (a, dim = 1).ne.3) STOP 7
  if (minval (a, dim = 1).ne.huge(h)-1) STOP 8
  c(:) = 5
  if (minloc (c, dim = 1).ne.1) STOP 9
  if (minval (c, dim = 1).ne.5) STOP 10
  c(2) = h
  if (minloc (c, dim = 1).ne.2) STOP 11
  if (minval (c, dim = 1).ne.h) STOP 12
  c(:) = huge(h)
  if (minloc (c, dim = 1).ne.1) STOP 13
  if (minval (c, dim = 1).ne.huge(h)) STOP 14
  c(3) = huge(h) - 1
  if (minloc (c, dim = 1).ne.3) STOP 15
  if (minval (c, dim = 1).ne.huge(h)-1) STOP 16
  l = .false.
  l2(:) = .false.
  a(:) = 5
  if (minloc (a, dim = 1, mask = l).ne.0) STOP 17
  if (minval (a, dim = 1, mask = l).ne.huge(h)) STOP 18
  if (minloc (a, dim = 1, mask = l2).ne.0) STOP 19
  if (minval (a, dim = 1, mask = l2).ne.huge(h)) STOP 20
  a(2) = h
  if (minloc (a, dim = 1, mask = l).ne.0) STOP 21
  if (minval (a, dim = 1, mask = l).ne.huge(h)) STOP 22
  if (minloc (a, dim = 1, mask = l2).ne.0) STOP 23
  if (minval (a, dim = 1, mask = l2).ne.huge(h)) STOP 24
  a(:) = huge(h)
  if (minloc (a, dim = 1, mask = l).ne.0) STOP 25
  if (minval (a, dim = 1, mask = l).ne.huge(h)) STOP 26
  if (minloc (a, dim = 1, mask = l2).ne.0) STOP 27
  if (minval (a, dim = 1, mask = l2).ne.huge(h)) STOP 28
  a(3) = huge(h) - 1
  if (minloc (a, dim = 1, mask = l).ne.0) STOP 29
  if (minval (a, dim = 1, mask = l).ne.huge(h)) STOP 30
  if (minloc (a, dim = 1, mask = l2).ne.0) STOP 31
  if (minval (a, dim = 1, mask = l2).ne.huge(h)) STOP 32
  c(:) = 5
  if (minloc (c, dim = 1, mask = l).ne.0) STOP 33
  if (minval (c, dim = 1, mask = l).ne.huge(h)) STOP 34
  if (minloc (c, dim = 1, mask = l2).ne.0) STOP 35
  if (minval (c, dim = 1, mask = l2).ne.huge(h)) STOP 36
  c(2) = h
  if (minloc (c, dim = 1, mask = l).ne.0) STOP 37
  if (minval (c, dim = 1, mask = l).ne.huge(h)) STOP 38
  if (minloc (c, dim = 1, mask = l2).ne.0) STOP 39
  if (minval (c, dim = 1, mask = l2).ne.huge(h)) STOP 40
  c(:) = huge(h)
  if (minloc (c, dim = 1, mask = l).ne.0) STOP 41
  if (minval (c, dim = 1, mask = l).ne.huge(h)) STOP 42
  if (minloc (c, dim = 1, mask = l2).ne.0) STOP 43
  if (minval (c, dim = 1, mask = l2).ne.huge(h)) STOP 44
  c(3) = huge(h) - 1
  if (minloc (c, dim = 1, mask = l).ne.0) STOP 45
  if (minval (c, dim = 1, mask = l).ne.huge(h)) STOP 46
  if (minloc (c, dim = 1, mask = l2).ne.0) STOP 47
  if (minval (c, dim = 1, mask = l2).ne.huge(h)) STOP 48
  l = .true.
  l2(:) = .true.
  a(:) = 5
  if (minloc (a, dim = 1, mask = l).ne.1) STOP 49
  if (minval (a, dim = 1, mask = l).ne.5) STOP 50
  if (minloc (a, dim = 1, mask = l2).ne.1) STOP 51
  if (minval (a, dim = 1, mask = l2).ne.5) STOP 52
  a(2) = h
  if (minloc (a, dim = 1, mask = l).ne.2) STOP 53
  if (minval (a, dim = 1, mask = l).ne.h) STOP 54
  if (minloc (a, dim = 1, mask = l2).ne.2) STOP 55
  if (minval (a, dim = 1, mask = l2).ne.h) STOP 56
  a(:) = huge(h)
  if (minloc (a, dim = 1, mask = l).ne.1) STOP 57
  if (minval (a, dim = 1, mask = l).ne.huge(h)) STOP 58
  if (minloc (a, dim = 1, mask = l2).ne.1) STOP 59
  if (minval (a, dim = 1, mask = l2).ne.huge(h)) STOP 60
  a(3) = huge(h) - 1
  if (minloc (a, dim = 1, mask = l).ne.3) STOP 61
  if (minval (a, dim = 1, mask = l).ne.huge(h)-1) STOP 62
  if (minloc (a, dim = 1, mask = l2).ne.3) STOP 63
  if (minval (a, dim = 1, mask = l2).ne.huge(h)-1) STOP 64
  c(:) = 5
  if (minloc (c, dim = 1, mask = l).ne.1) STOP 65
  if (minval (c, dim = 1, mask = l).ne.5) STOP 66
  if (minloc (c, dim = 1, mask = l2).ne.1) STOP 67
  if (minval (c, dim = 1, mask = l2).ne.5) STOP 68
  c(2) = h
  if (minloc (c, dim = 1, mask = l).ne.2) STOP 69
  if (minval (c, dim = 1, mask = l).ne.h) STOP 70
  if (minloc (c, dim = 1, mask = l2).ne.2) STOP 71
  if (minval (c, dim = 1, mask = l2).ne.h) STOP 72
  c(:) = huge(h)
  if (minloc (c, dim = 1, mask = l).ne.1) STOP 73
  if (minval (c, dim = 1, mask = l).ne.huge(h)) STOP 74
  if (minloc (c, dim = 1, mask = l2).ne.1) STOP 75
  if (minval (c, dim = 1, mask = l2).ne.huge(h)) STOP 76
  c(3) = huge(h) - 1
  if (minloc (c, dim = 1, mask = l).ne.3) STOP 77
  if (minval (c, dim = 1, mask = l).ne.huge(h)-1) STOP 78
  if (minloc (c, dim = 1, mask = l2).ne.3) STOP 79
  if (minval (c, dim = 1, mask = l2).ne.huge(h)-1) STOP 80
  deallocate (c)
  allocate (c(-2:-3))
  if (minloc (c, dim = 1).ne.0) STOP 81
  if (minval (c, dim = 1).ne.huge(h)) STOP 82
end
