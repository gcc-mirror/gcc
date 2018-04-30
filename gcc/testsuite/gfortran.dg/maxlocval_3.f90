! { dg-do run }
  integer :: a(3), h
  integer, allocatable :: c(:)
  logical :: l
  logical :: l2(3)

  h = -huge(h)
  h = h - 1
  allocate (c(3))
  a(:) = 5
  if (maxloc (a, dim = 1).ne.1) STOP 1
  if (maxval (a, dim = 1).ne.5) STOP 2
  a(2) = huge(h)
  if (maxloc (a, dim = 1).ne.2) STOP 3
  if (maxval (a, dim = 1).ne.huge(h)) STOP 4
  a(:) = h
  if (maxloc (a, dim = 1).ne.1) STOP 5
  if (maxval (a, dim = 1).ne.h) STOP 6
  a(3) = -huge(h)
  if (maxloc (a, dim = 1).ne.3) STOP 7
  if (maxval (a, dim = 1).ne.-huge(h)) STOP 8
  c(:) = 5
  if (maxloc (c, dim = 1).ne.1) STOP 9
  if (maxval (c, dim = 1).ne.5) STOP 10
  c(2) = huge(h)
  if (maxloc (c, dim = 1).ne.2) STOP 11
  if (maxval (c, dim = 1).ne.huge(h)) STOP 12
  c(:) = h
  if (maxloc (c, dim = 1).ne.1) STOP 13
  if (maxval (c, dim = 1).ne.h) STOP 14
  c(3) = -huge(h)
  if (maxloc (c, dim = 1).ne.3) STOP 15
  if (maxval (c, dim = 1).ne.-huge(h)) STOP 16
  l = .false.
  l2(:) = .false.
  a(:) = 5
  if (maxloc (a, dim = 1, mask = l).ne.0) STOP 17
  if (maxval (a, dim = 1, mask = l).ne.h) STOP 18
  if (maxloc (a, dim = 1, mask = l2).ne.0) STOP 19
  if (maxval (a, dim = 1, mask = l2).ne.h) STOP 20
  a(2) = huge(h)
  if (maxloc (a, dim = 1, mask = l).ne.0) STOP 21
  if (maxval (a, dim = 1, mask = l).ne.h) STOP 22
  if (maxloc (a, dim = 1, mask = l2).ne.0) STOP 23
  if (maxval (a, dim = 1, mask = l2).ne.h) STOP 24
  a(:) = h
  if (maxloc (a, dim = 1, mask = l).ne.0) STOP 25
  if (maxval (a, dim = 1, mask = l).ne.h) STOP 26
  if (maxloc (a, dim = 1, mask = l2).ne.0) STOP 27
  if (maxval (a, dim = 1, mask = l2).ne.h) STOP 28
  a(3) = -huge(h)
  if (maxloc (a, dim = 1, mask = l).ne.0) STOP 29
  if (maxval (a, dim = 1, mask = l).ne.h) STOP 30
  if (maxloc (a, dim = 1, mask = l2).ne.0) STOP 31
  if (maxval (a, dim = 1, mask = l2).ne.h) STOP 32
  c(:) = 5
  if (maxloc (c, dim = 1, mask = l).ne.0) STOP 33
  if (maxval (c, dim = 1, mask = l).ne.h) STOP 34
  if (maxloc (c, dim = 1, mask = l2).ne.0) STOP 35
  if (maxval (c, dim = 1, mask = l2).ne.h) STOP 36
  c(2) = huge(h)
  if (maxloc (c, dim = 1, mask = l).ne.0) STOP 37
  if (maxval (c, dim = 1, mask = l).ne.h) STOP 38
  if (maxloc (c, dim = 1, mask = l2).ne.0) STOP 39
  if (maxval (c, dim = 1, mask = l2).ne.h) STOP 40
  c(:) = h
  if (maxloc (c, dim = 1, mask = l).ne.0) STOP 41
  if (maxval (c, dim = 1, mask = l).ne.h) STOP 42
  if (maxloc (c, dim = 1, mask = l2).ne.0) STOP 43
  if (maxval (c, dim = 1, mask = l2).ne.h) STOP 44
  c(3) = -huge(h)
  if (maxloc (c, dim = 1, mask = l).ne.0) STOP 45
  if (maxval (c, dim = 1, mask = l).ne.h) STOP 46
  if (maxloc (c, dim = 1, mask = l2).ne.0) STOP 47
  if (maxval (c, dim = 1, mask = l2).ne.h) STOP 48
  l = .true.
  l2(:) = .true.
  a(:) = 5
  if (maxloc (a, dim = 1, mask = l).ne.1) STOP 49
  if (maxval (a, dim = 1, mask = l).ne.5) STOP 50
  if (maxloc (a, dim = 1, mask = l2).ne.1) STOP 51
  if (maxval (a, dim = 1, mask = l2).ne.5) STOP 52
  a(2) = huge(h)
  if (maxloc (a, dim = 1, mask = l).ne.2) STOP 53
  if (maxval (a, dim = 1, mask = l).ne.huge(h)) STOP 54
  if (maxloc (a, dim = 1, mask = l2).ne.2) STOP 55
  if (maxval (a, dim = 1, mask = l2).ne.huge(h)) STOP 56
  a(:) = h
  if (maxloc (a, dim = 1, mask = l).ne.1) STOP 57
  if (maxval (a, dim = 1, mask = l).ne.h) STOP 58
  if (maxloc (a, dim = 1, mask = l2).ne.1) STOP 59
  if (maxval (a, dim = 1, mask = l2).ne.h) STOP 60
  a(3) = -huge(h)
  if (maxloc (a, dim = 1, mask = l).ne.3) STOP 61
  if (maxval (a, dim = 1, mask = l).ne.-huge(h)) STOP 62
  if (maxloc (a, dim = 1, mask = l2).ne.3) STOP 63
  if (maxval (a, dim = 1, mask = l2).ne.-huge(h)) STOP 64
  c(:) = 5
  if (maxloc (c, dim = 1, mask = l).ne.1) STOP 65
  if (maxval (c, dim = 1, mask = l).ne.5) STOP 66
  if (maxloc (c, dim = 1, mask = l2).ne.1) STOP 67
  if (maxval (c, dim = 1, mask = l2).ne.5) STOP 68
  c(2) = huge(h)
  if (maxloc (c, dim = 1, mask = l).ne.2) STOP 69
  if (maxval (c, dim = 1, mask = l).ne.huge(h)) STOP 70
  if (maxloc (c, dim = 1, mask = l2).ne.2) STOP 71
  if (maxval (c, dim = 1, mask = l2).ne.huge(h)) STOP 72
  c(:) = h
  if (maxloc (c, dim = 1, mask = l).ne.1) STOP 73
  if (maxval (c, dim = 1, mask = l).ne.h) STOP 74
  if (maxloc (c, dim = 1, mask = l2).ne.1) STOP 75
  if (maxval (c, dim = 1, mask = l2).ne.h) STOP 76
  c(3) = -huge(h)
  if (maxloc (c, dim = 1, mask = l).ne.3) STOP 77
  if (maxval (c, dim = 1, mask = l).ne.-huge(h)) STOP 78
  if (maxloc (c, dim = 1, mask = l2).ne.3) STOP 79
  if (maxval (c, dim = 1, mask = l2).ne.-huge(h)) STOP 80
  deallocate (c)
  allocate (c(-2:-3))
  if (maxloc (c, dim = 1).ne.0) STOP 81
  if (maxval (c, dim = 1).ne.h) STOP 82
end
