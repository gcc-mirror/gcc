! PR fortran/120191
! { dg-do run }

  integer(kind=1) :: a1(10, 10, 10), b1(10)
  integer(kind=2) :: a2(10, 10, 10), b2(10)
  integer(kind=4) :: a4(10, 10, 10), b4(10)
  integer(kind=8) :: a8(10, 10, 10), b8(10)
  real(kind=4) :: r4(10, 10, 10), s4(10)
  real(kind=8) :: r8(10, 10, 10), s8(10)
  logical :: l1(10, 10, 10), l2(10), l3
  l1 = .true.
  l2 = .true.
  l3 = .true.
  a1 = 0
  if (any (maxloc (a1) .ne. 1)) stop 1
  if (any (maxloc (a1, back=.false.) .ne. 1)) stop 2
  if (any (maxloc (a1, back=.true.) .ne. 10)) stop 3
  if (any (maxloc (a1, kind=2) .ne. 1)) stop 4
  if (any (maxloc (a1, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (maxloc (a1, kind=8, back=.true.) .ne. 10)) stop 6
  if (any (maxloc (a1, 1) .ne. 1)) stop 7
  if (any (maxloc (a1, 1, back=.false.) .ne. 1)) stop 8
  if (any (maxloc (a1, 1, back=.true.) .ne. 10)) stop 9
  if (any (maxloc (a1, 1, kind=1) .ne. 1)) stop 10
  if (any (maxloc (a1, 1, kind=2, back=.false.) .ne. 1)) stop 11
  if (any (maxloc (a1, 1, kind=4, back=.true.) .ne. 10)) stop 12
  if (any (maxloc (a1, 1, l1) .ne. 1)) stop 13
  if (any (maxloc (a1, 1, l1, back=.false.) .ne. 1)) stop 14
  if (any (maxloc (a1, 1, l1, back=.true.) .ne. 10)) stop 15
  if (any (maxloc (a1, 1, l1, kind=8) .ne. 1)) stop 16
  if (any (maxloc (a1, 1, l1, 4, .false.) .ne. 1)) stop 17
  if (any (maxloc (a1, 1, l1, 2, .true.) .ne. 10)) stop 18
  if (any (maxloc (a1, 1, l3) .ne. 1)) stop 19
  if (any (maxloc (a1, 1, l3, back=.false.) .ne. 1)) stop 20
  if (any (maxloc (a1, 1, l3, back=.true.) .ne. 10)) stop 21
  if (any (maxloc (a1, 1, l3, kind=8) .ne. 1)) stop 22
  if (any (maxloc (a1, 1, l3, 4, .false.) .ne. 1)) stop 23
  if (any (maxloc (a1, 1, l3, 2, .true.) .ne. 10)) stop 24
  b1 = 0
  if (any (maxloc (b1) .ne. 1)) stop 1
  if (any (maxloc (b1, back=.false.) .ne. 1)) stop 2
  if (any (maxloc (b1, back=.true.) .ne. 10)) stop 3
  if (any (maxloc (b1, kind=2) .ne. 1)) stop 4
  if (any (maxloc (b1, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (maxloc (b1, kind=8, back=.true.) .ne. 10)) stop 6
  if (maxloc (b1, 1) .ne. 1) stop 7
  if (maxloc (b1, 1, back=.false.) .ne. 1) stop 8
  if (maxloc (b1, 1, back=.true.) .ne. 10) stop 9
  if (maxloc (b1, 1, kind=1) .ne. 1) stop 10
  if (maxloc (b1, 1, kind=2, back=.false.) .ne. 1) stop 11
  if (maxloc (b1, 1, kind=4, back=.true.) .ne. 10) stop 12
  if (maxloc (b1, 1, l2) .ne. 1) stop 13
  if (maxloc (b1, 1, l2, back=.false.) .ne. 1) stop 14
  if (maxloc (b1, 1, l2, back=.true.) .ne. 10) stop 15
  if (maxloc (b1, 1, l2, kind=8) .ne. 1) stop 16
  if (maxloc (b1, 1, l2, 4, .false.) .ne. 1) stop 17
  if (maxloc (b1, 1, l2, 2, .true.) .ne. 10) stop 18
  if (maxloc (b1, 1, l3) .ne. 1) stop 19
  if (maxloc (b1, 1, l3, back=.false.) .ne. 1) stop 20
  if (maxloc (b1, 1, l3, back=.true.) .ne. 10) stop 21
  if (maxloc (b1, 1, l3, kind=8) .ne. 1) stop 22
  if (maxloc (b1, 1, l3, 4, .false.) .ne. 1) stop 23
  if (maxloc (b1, 1, l3, 2, .true.) .ne. 10) stop 24
  a2 = 0
  if (any (maxloc (a2) .ne. 1)) stop 1
  if (any (maxloc (a2, back=.false.) .ne. 1)) stop 2
  if (any (maxloc (a2, back=.true.) .ne. 10)) stop 3
  if (any (maxloc (a2, kind=2) .ne. 1)) stop 4
  if (any (maxloc (a2, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (maxloc (a2, kind=8, back=.true.) .ne. 10)) stop 6
  if (any (maxloc (a2, 1) .ne. 1)) stop 7
  if (any (maxloc (a2, 1, back=.false.) .ne. 1)) stop 8
  if (any (maxloc (a2, 1, back=.true.) .ne. 10)) stop 9
  if (any (maxloc (a2, 1, kind=1) .ne. 1)) stop 10
  if (any (maxloc (a2, 1, kind=2, back=.false.) .ne. 1)) stop 11
  if (any (maxloc (a2, 1, kind=4, back=.true.) .ne. 10)) stop 12
  if (any (maxloc (a2, 1, l1) .ne. 1)) stop 13
  if (any (maxloc (a2, 1, l1, back=.false.) .ne. 1)) stop 14
  if (any (maxloc (a2, 1, l1, back=.true.) .ne. 10)) stop 15
  if (any (maxloc (a2, 1, l1, kind=8) .ne. 1)) stop 16
  if (any (maxloc (a2, 1, l1, 4, .false.) .ne. 1)) stop 17
  if (any (maxloc (a2, 1, l1, 2, .true.) .ne. 10)) stop 18
  if (any (maxloc (a2, 1, l3) .ne. 1)) stop 19
  if (any (maxloc (a2, 1, l3, back=.false.) .ne. 1)) stop 20
  if (any (maxloc (a2, 1, l3, back=.true.) .ne. 10)) stop 21
  if (any (maxloc (a2, 1, l3, kind=8) .ne. 1)) stop 22
  if (any (maxloc (a2, 1, l3, 4, .false.) .ne. 1)) stop 23
  if (any (maxloc (a2, 1, l3, 2, .true.) .ne. 10)) stop 24
  b2 = 0
  if (any (maxloc (b2) .ne. 1)) stop 1
  if (any (maxloc (b2, back=.false.) .ne. 1)) stop 2
  if (any (maxloc (b2, back=.true.) .ne. 10)) stop 3
  if (any (maxloc (b2, kind=2) .ne. 1)) stop 4
  if (any (maxloc (b2, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (maxloc (b2, kind=8, back=.true.) .ne. 10)) stop 6
  if (maxloc (b2, 1) .ne. 1) stop 7
  if (maxloc (b2, 1, back=.false.) .ne. 1) stop 8
  if (maxloc (b2, 1, back=.true.) .ne. 10) stop 9
  if (maxloc (b2, 1, kind=1) .ne. 1) stop 10
  if (maxloc (b2, 1, kind=2, back=.false.) .ne. 1) stop 11
  if (maxloc (b2, 1, kind=4, back=.true.) .ne. 10) stop 12
  if (maxloc (b2, 1, l2) .ne. 1) stop 13
  if (maxloc (b2, 1, l2, back=.false.) .ne. 1) stop 14
  if (maxloc (b2, 1, l2, back=.true.) .ne. 10) stop 15
  if (maxloc (b2, 1, l2, kind=8) .ne. 1) stop 16
  if (maxloc (b2, 1, l2, 4, .false.) .ne. 1) stop 17
  if (maxloc (b2, 1, l2, 2, .true.) .ne. 10) stop 18
  if (maxloc (b2, 1, l3) .ne. 1) stop 19
  if (maxloc (b2, 1, l3, back=.false.) .ne. 1) stop 20
  if (maxloc (b2, 1, l3, back=.true.) .ne. 10) stop 21
  if (maxloc (b2, 1, l3, kind=8) .ne. 1) stop 22
  if (maxloc (b2, 1, l3, 4, .false.) .ne. 1) stop 23
  if (maxloc (b2, 1, l3, 2, .true.) .ne. 10) stop 24
  a4 = 0
  if (any (maxloc (a4) .ne. 1)) stop 1
  if (any (maxloc (a4, back=.false.) .ne. 1)) stop 2
  if (any (maxloc (a4, back=.true.) .ne. 10)) stop 3
  if (any (maxloc (a4, kind=2) .ne. 1)) stop 4
  if (any (maxloc (a4, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (maxloc (a4, kind=8, back=.true.) .ne. 10)) stop 6
  if (any (maxloc (a4, 1) .ne. 1)) stop 7
  if (any (maxloc (a4, 1, back=.false.) .ne. 1)) stop 8
  if (any (maxloc (a4, 1, back=.true.) .ne. 10)) stop 9
  if (any (maxloc (a4, 1, kind=1) .ne. 1)) stop 10
  if (any (maxloc (a4, 1, kind=2, back=.false.) .ne. 1)) stop 11
  if (any (maxloc (a4, 1, kind=4, back=.true.) .ne. 10)) stop 12
  if (any (maxloc (a4, 1, l1) .ne. 1)) stop 13
  if (any (maxloc (a4, 1, l1, back=.false.) .ne. 1)) stop 14
  if (any (maxloc (a4, 1, l1, back=.true.) .ne. 10)) stop 15
  if (any (maxloc (a4, 1, l1, kind=8) .ne. 1)) stop 16
  if (any (maxloc (a4, 1, l1, 4, .false.) .ne. 1)) stop 17
  if (any (maxloc (a4, 1, l1, 2, .true.) .ne. 10)) stop 18
  if (any (maxloc (a4, 1, l3) .ne. 1)) stop 19
  if (any (maxloc (a4, 1, l3, back=.false.) .ne. 1)) stop 20
  if (any (maxloc (a4, 1, l3, back=.true.) .ne. 10)) stop 21
  if (any (maxloc (a4, 1, l3, kind=8) .ne. 1)) stop 22
  if (any (maxloc (a4, 1, l3, 4, .false.) .ne. 1)) stop 23
  if (any (maxloc (a4, 1, l3, 2, .true.) .ne. 10)) stop 24
  b4 = 0
  if (any (maxloc (b4) .ne. 1)) stop 1
  if (any (maxloc (b4, back=.false.) .ne. 1)) stop 2
  if (any (maxloc (b4, back=.true.) .ne. 10)) stop 3
  if (any (maxloc (b4, kind=2) .ne. 1)) stop 4
  if (any (maxloc (b4, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (maxloc (b4, kind=8, back=.true.) .ne. 10)) stop 6
  if (maxloc (b4, 1) .ne. 1) stop 7
  if (maxloc (b4, 1, back=.false.) .ne. 1) stop 8
  if (maxloc (b4, 1, back=.true.) .ne. 10) stop 9
  if (maxloc (b4, 1, kind=1) .ne. 1) stop 10
  if (maxloc (b4, 1, kind=2, back=.false.) .ne. 1) stop 11
  if (maxloc (b4, 1, kind=4, back=.true.) .ne. 10) stop 12
  if (maxloc (b4, 1, l2) .ne. 1) stop 13
  if (maxloc (b4, 1, l2, back=.false.) .ne. 1) stop 14
  if (maxloc (b4, 1, l2, back=.true.) .ne. 10) stop 15
  if (maxloc (b4, 1, l2, kind=8) .ne. 1) stop 16
  if (maxloc (b4, 1, l2, 4, .false.) .ne. 1) stop 17
  if (maxloc (b4, 1, l2, 2, .true.) .ne. 10) stop 18
  if (maxloc (b4, 1, l3) .ne. 1) stop 19
  if (maxloc (b4, 1, l3, back=.false.) .ne. 1) stop 20
  if (maxloc (b4, 1, l3, back=.true.) .ne. 10) stop 21
  if (maxloc (b4, 1, l3, kind=8) .ne. 1) stop 22
  if (maxloc (b4, 1, l3, 4, .false.) .ne. 1) stop 23
  if (maxloc (b4, 1, l3, 2, .true.) .ne. 10) stop 24
  a8 = 0
  if (any (maxloc (a8) .ne. 1)) stop 1
  if (any (maxloc (a8, back=.false.) .ne. 1)) stop 2
  if (any (maxloc (a8, back=.true.) .ne. 10)) stop 3
  if (any (maxloc (a8, kind=2) .ne. 1)) stop 4
  if (any (maxloc (a8, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (maxloc (a8, kind=8, back=.true.) .ne. 10)) stop 6
  if (any (maxloc (a8, 1) .ne. 1)) stop 7
  if (any (maxloc (a8, 1, back=.false.) .ne. 1)) stop 8
  if (any (maxloc (a8, 1, back=.true.) .ne. 10)) stop 9
  if (any (maxloc (a8, 1, kind=1) .ne. 1)) stop 10
  if (any (maxloc (a8, 1, kind=2, back=.false.) .ne. 1)) stop 11
  if (any (maxloc (a8, 1, kind=4, back=.true.) .ne. 10)) stop 12
  if (any (maxloc (a8, 1, l1) .ne. 1)) stop 13
  if (any (maxloc (a8, 1, l1, back=.false.) .ne. 1)) stop 14
  if (any (maxloc (a8, 1, l1, back=.true.) .ne. 10)) stop 15
  if (any (maxloc (a8, 1, l1, kind=8) .ne. 1)) stop 16
  if (any (maxloc (a8, 1, l1, 4, .false.) .ne. 1)) stop 17
  if (any (maxloc (a8, 1, l1, 2, .true.) .ne. 10)) stop 18
  if (any (maxloc (a8, 1, l3) .ne. 1)) stop 19
  if (any (maxloc (a8, 1, l3, back=.false.) .ne. 1)) stop 20
  if (any (maxloc (a8, 1, l3, back=.true.) .ne. 10)) stop 21
  if (any (maxloc (a8, 1, l3, kind=8) .ne. 1)) stop 22
  if (any (maxloc (a8, 1, l3, 4, .false.) .ne. 1)) stop 23
  if (any (maxloc (a8, 1, l3, 2, .true.) .ne. 10)) stop 24
  b8 = 0
  if (any (maxloc (b8) .ne. 1)) stop 1
  if (any (maxloc (b8, back=.false.) .ne. 1)) stop 2
  if (any (maxloc (b8, back=.true.) .ne. 10)) stop 3
  if (any (maxloc (b8, kind=2) .ne. 1)) stop 4
  if (any (maxloc (b8, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (maxloc (b8, kind=8, back=.true.) .ne. 10)) stop 6
  if (maxloc (b8, 1) .ne. 1) stop 7
  if (maxloc (b8, 1, back=.false.) .ne. 1) stop 8
  if (maxloc (b8, 1, back=.true.) .ne. 10) stop 9
  if (maxloc (b8, 1, kind=1) .ne. 1) stop 10
  if (maxloc (b8, 1, kind=2, back=.false.) .ne. 1) stop 11
  if (maxloc (b8, 1, kind=4, back=.true.) .ne. 10) stop 12
  if (maxloc (b8, 1, l2) .ne. 1) stop 13
  if (maxloc (b8, 1, l2, back=.false.) .ne. 1) stop 14
  if (maxloc (b8, 1, l2, back=.true.) .ne. 10) stop 15
  if (maxloc (b8, 1, l2, kind=8) .ne. 1) stop 16
  if (maxloc (b8, 1, l2, 4, .false.) .ne. 1) stop 17
  if (maxloc (b8, 1, l2, 2, .true.) .ne. 10) stop 18
  if (maxloc (b8, 1, l3) .ne. 1) stop 19
  if (maxloc (b8, 1, l3, back=.false.) .ne. 1) stop 20
  if (maxloc (b8, 1, l3, back=.true.) .ne. 10) stop 21
  if (maxloc (b8, 1, l3, kind=8) .ne. 1) stop 22
  if (maxloc (b8, 1, l3, 4, .false.) .ne. 1) stop 23
  if (maxloc (b8, 1, l3, 2, .true.) .ne. 10) stop 24
  r4 = 0.0
  if (any (maxloc (r4) .ne. 1)) stop 1
  if (any (maxloc (r4, back=.false.) .ne. 1)) stop 2
  if (any (maxloc (r4, back=.true.) .ne. 10)) stop 3
  if (any (maxloc (r4, kind=2) .ne. 1)) stop 4
  if (any (maxloc (r4, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (maxloc (r4, kind=8, back=.true.) .ne. 10)) stop 6
  if (any (maxloc (r4, 1) .ne. 1)) stop 7
  if (any (maxloc (r4, 1, back=.false.) .ne. 1)) stop 8
  if (any (maxloc (r4, 1, back=.true.) .ne. 10)) stop 9
  if (any (maxloc (r4, 1, kind=1) .ne. 1)) stop 10
  if (any (maxloc (r4, 1, kind=2, back=.false.) .ne. 1)) stop 11
  if (any (maxloc (r4, 1, kind=4, back=.true.) .ne. 10)) stop 12
  if (any (maxloc (r4, 1, l1) .ne. 1)) stop 13
  if (any (maxloc (r4, 1, l1, back=.false.) .ne. 1)) stop 14
  if (any (maxloc (r4, 1, l1, back=.true.) .ne. 10)) stop 15
  if (any (maxloc (r4, 1, l1, kind=8) .ne. 1)) stop 16
  if (any (maxloc (r4, 1, l1, 4, .false.) .ne. 1)) stop 17
  if (any (maxloc (r4, 1, l1, 2, .true.) .ne. 10)) stop 18
  if (any (maxloc (r4, 1, l3) .ne. 1)) stop 19
  if (any (maxloc (r4, 1, l3, back=.false.) .ne. 1)) stop 20
  if (any (maxloc (r4, 1, l3, back=.true.) .ne. 10)) stop 21
  if (any (maxloc (r4, 1, l3, kind=8) .ne. 1)) stop 22
  if (any (maxloc (r4, 1, l3, 4, .false.) .ne. 1)) stop 23
  if (any (maxloc (r4, 1, l3, 2, .true.) .ne. 10)) stop 24
  s4 = 0.0
  if (any (maxloc (s4) .ne. 1)) stop 1
  if (any (maxloc (s4, back=.false.) .ne. 1)) stop 2
  if (any (maxloc (s4, back=.true.) .ne. 10)) stop 3
  if (any (maxloc (s4, kind=2) .ne. 1)) stop 4
  if (any (maxloc (s4, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (maxloc (s4, kind=8, back=.true.) .ne. 10)) stop 6
  if (maxloc (s4, 1) .ne. 1) stop 7
  if (maxloc (s4, 1, back=.false.) .ne. 1) stop 8
  if (maxloc (s4, 1, back=.true.) .ne. 10) stop 9
  if (maxloc (s4, 1, kind=1) .ne. 1) stop 10
  if (maxloc (s4, 1, kind=2, back=.false.) .ne. 1) stop 11
  if (maxloc (s4, 1, kind=4, back=.true.) .ne. 10) stop 12
  if (maxloc (s4, 1, l2) .ne. 1) stop 13
  if (maxloc (s4, 1, l2, back=.false.) .ne. 1) stop 14
  if (maxloc (s4, 1, l2, back=.true.) .ne. 10) stop 15
  if (maxloc (s4, 1, l2, kind=8) .ne. 1) stop 16
  if (maxloc (s4, 1, l2, 4, .false.) .ne. 1) stop 17
  if (maxloc (s4, 1, l2, 2, .true.) .ne. 10) stop 18
  if (maxloc (s4, 1, l3) .ne. 1) stop 19
  if (maxloc (s4, 1, l3, back=.false.) .ne. 1) stop 20
  if (maxloc (s4, 1, l3, back=.true.) .ne. 10) stop 21
  if (maxloc (s4, 1, l3, kind=8) .ne. 1) stop 22
  if (maxloc (s4, 1, l3, 4, .false.) .ne. 1) stop 23
  if (maxloc (s4, 1, l3, 2, .true.) .ne. 10) stop 24
  r8 = 0.0
  if (any (maxloc (r8) .ne. 1)) stop 1
  if (any (maxloc (r8, back=.false.) .ne. 1)) stop 2
  if (any (maxloc (r8, back=.true.) .ne. 10)) stop 3
  if (any (maxloc (r8, kind=2) .ne. 1)) stop 4
  if (any (maxloc (r8, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (maxloc (r8, kind=8, back=.true.) .ne. 10)) stop 6
  if (any (maxloc (r8, 1) .ne. 1)) stop 7
  if (any (maxloc (r8, 1, back=.false.) .ne. 1)) stop 8
  if (any (maxloc (r8, 1, back=.true.) .ne. 10)) stop 9
  if (any (maxloc (r8, 1, kind=1) .ne. 1)) stop 10
  if (any (maxloc (r8, 1, kind=2, back=.false.) .ne. 1)) stop 11
  if (any (maxloc (r8, 1, kind=4, back=.true.) .ne. 10)) stop 12
  if (any (maxloc (r8, 1, l1) .ne. 1)) stop 13
  if (any (maxloc (r8, 1, l1, back=.false.) .ne. 1)) stop 14
  if (any (maxloc (r8, 1, l1, back=.true.) .ne. 10)) stop 15
  if (any (maxloc (r8, 1, l1, kind=8) .ne. 1)) stop 16
  if (any (maxloc (r8, 1, l1, 4, .false.) .ne. 1)) stop 17
  if (any (maxloc (r8, 1, l1, 2, .true.) .ne. 10)) stop 18
  if (any (maxloc (r8, 1, l3) .ne. 1)) stop 19
  if (any (maxloc (r8, 1, l3, back=.false.) .ne. 1)) stop 20
  if (any (maxloc (r8, 1, l3, back=.true.) .ne. 10)) stop 21
  if (any (maxloc (r8, 1, l3, kind=8) .ne. 1)) stop 22
  if (any (maxloc (r8, 1, l3, 4, .false.) .ne. 1)) stop 23
  if (any (maxloc (r8, 1, l3, 2, .true.) .ne. 10)) stop 24
  s8 = 0.0
  if (any (maxloc (s8) .ne. 1)) stop 1
  if (any (maxloc (s8, back=.false.) .ne. 1)) stop 2
  if (any (maxloc (s8, back=.true.) .ne. 10)) stop 3
  if (any (maxloc (s8, kind=2) .ne. 1)) stop 4
  if (any (maxloc (s8, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (maxloc (s8, kind=8, back=.true.) .ne. 10)) stop 6
  if (maxloc (s8, 1) .ne. 1) stop 7
  if (maxloc (s8, 1, back=.false.) .ne. 1) stop 8
  if (maxloc (s8, 1, back=.true.) .ne. 10) stop 9
  if (maxloc (s8, 1, kind=1) .ne. 1) stop 10
  if (maxloc (s8, 1, kind=2, back=.false.) .ne. 1) stop 11
  if (maxloc (s8, 1, kind=4, back=.true.) .ne. 10) stop 12
  if (maxloc (s8, 1, l2) .ne. 1) stop 13
  if (maxloc (s8, 1, l2, back=.false.) .ne. 1) stop 14
  if (maxloc (s8, 1, l2, back=.true.) .ne. 10) stop 15
  if (maxloc (s8, 1, l2, kind=8) .ne. 1) stop 16
  if (maxloc (s8, 1, l2, 4, .false.) .ne. 1) stop 17
  if (maxloc (s8, 1, l2, 2, .true.) .ne. 10) stop 18
  if (maxloc (s8, 1, l3) .ne. 1) stop 19
  if (maxloc (s8, 1, l3, back=.false.) .ne. 1) stop 20
  if (maxloc (s8, 1, l3, back=.true.) .ne. 10) stop 21
  if (maxloc (s8, 1, l3, kind=8) .ne. 1) stop 22
  if (maxloc (s8, 1, l3, 4, .false.) .ne. 1) stop 23
  if (maxloc (s8, 1, l3, 2, .true.) .ne. 10) stop 24
  a1 = 0
  if (any (minloc (a1) .ne. 1)) stop 1
  if (any (minloc (a1, back=.false.) .ne. 1)) stop 2
  if (any (minloc (a1, back=.true.) .ne. 10)) stop 3
  if (any (minloc (a1, kind=2) .ne. 1)) stop 4
  if (any (minloc (a1, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (minloc (a1, kind=8, back=.true.) .ne. 10)) stop 6
  if (any (minloc (a1, 1) .ne. 1)) stop 7
  if (any (minloc (a1, 1, back=.false.) .ne. 1)) stop 8
  if (any (minloc (a1, 1, back=.true.) .ne. 10)) stop 9
  if (any (minloc (a1, 1, kind=1) .ne. 1)) stop 10
  if (any (minloc (a1, 1, kind=2, back=.false.) .ne. 1)) stop 11
  if (any (minloc (a1, 1, kind=4, back=.true.) .ne. 10)) stop 12
  if (any (minloc (a1, 1, l1) .ne. 1)) stop 13
  if (any (minloc (a1, 1, l1, back=.false.) .ne. 1)) stop 14
  if (any (minloc (a1, 1, l1, back=.true.) .ne. 10)) stop 15
  if (any (minloc (a1, 1, l1, kind=8) .ne. 1)) stop 16
  if (any (minloc (a1, 1, l1, 4, .false.) .ne. 1)) stop 17
  if (any (minloc (a1, 1, l1, 2, .true.) .ne. 10)) stop 18
  if (any (minloc (a1, 1, l3) .ne. 1)) stop 19
  if (any (minloc (a1, 1, l3, back=.false.) .ne. 1)) stop 20
  if (any (minloc (a1, 1, l3, back=.true.) .ne. 10)) stop 21
  if (any (minloc (a1, 1, l3, kind=8) .ne. 1)) stop 22
  if (any (minloc (a1, 1, l3, 4, .false.) .ne. 1)) stop 23
  if (any (minloc (a1, 1, l3, 2, .true.) .ne. 10)) stop 24
  b1 = 0
  if (any (minloc (b1) .ne. 1)) stop 1
  if (any (minloc (b1, back=.false.) .ne. 1)) stop 2
  if (any (minloc (b1, back=.true.) .ne. 10)) stop 3
  if (any (minloc (b1, kind=2) .ne. 1)) stop 4
  if (any (minloc (b1, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (minloc (b1, kind=8, back=.true.) .ne. 10)) stop 6
  if (minloc (b1, 1) .ne. 1) stop 7
  if (minloc (b1, 1, back=.false.) .ne. 1) stop 8
  if (minloc (b1, 1, back=.true.) .ne. 10) stop 9
  if (minloc (b1, 1, kind=1) .ne. 1) stop 10
  if (minloc (b1, 1, kind=2, back=.false.) .ne. 1) stop 11
  if (minloc (b1, 1, kind=4, back=.true.) .ne. 10) stop 12
  if (minloc (b1, 1, l2) .ne. 1) stop 13
  if (minloc (b1, 1, l2, back=.false.) .ne. 1) stop 14
  if (minloc (b1, 1, l2, back=.true.) .ne. 10) stop 15
  if (minloc (b1, 1, l2, kind=8) .ne. 1) stop 16
  if (minloc (b1, 1, l2, 4, .false.) .ne. 1) stop 17
  if (minloc (b1, 1, l2, 2, .true.) .ne. 10) stop 18
  if (minloc (b1, 1, l3) .ne. 1) stop 19
  if (minloc (b1, 1, l3, back=.false.) .ne. 1) stop 20
  if (minloc (b1, 1, l3, back=.true.) .ne. 10) stop 21
  if (minloc (b1, 1, l3, kind=8) .ne. 1) stop 22
  if (minloc (b1, 1, l3, 4, .false.) .ne. 1) stop 23
  if (minloc (b1, 1, l3, 2, .true.) .ne. 10) stop 24
  a2 = 0
  if (any (minloc (a2) .ne. 1)) stop 1
  if (any (minloc (a2, back=.false.) .ne. 1)) stop 2
  if (any (minloc (a2, back=.true.) .ne. 10)) stop 3
  if (any (minloc (a2, kind=2) .ne. 1)) stop 4
  if (any (minloc (a2, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (minloc (a2, kind=8, back=.true.) .ne. 10)) stop 6
  if (any (minloc (a2, 1) .ne. 1)) stop 7
  if (any (minloc (a2, 1, back=.false.) .ne. 1)) stop 8
  if (any (minloc (a2, 1, back=.true.) .ne. 10)) stop 9
  if (any (minloc (a2, 1, kind=1) .ne. 1)) stop 10
  if (any (minloc (a2, 1, kind=2, back=.false.) .ne. 1)) stop 11
  if (any (minloc (a2, 1, kind=4, back=.true.) .ne. 10)) stop 12
  if (any (minloc (a2, 1, l1) .ne. 1)) stop 13
  if (any (minloc (a2, 1, l1, back=.false.) .ne. 1)) stop 14
  if (any (minloc (a2, 1, l1, back=.true.) .ne. 10)) stop 15
  if (any (minloc (a2, 1, l1, kind=8) .ne. 1)) stop 16
  if (any (minloc (a2, 1, l1, 4, .false.) .ne. 1)) stop 17
  if (any (minloc (a2, 1, l1, 2, .true.) .ne. 10)) stop 18
  if (any (minloc (a2, 1, l3) .ne. 1)) stop 19
  if (any (minloc (a2, 1, l3, back=.false.) .ne. 1)) stop 20
  if (any (minloc (a2, 1, l3, back=.true.) .ne. 10)) stop 21
  if (any (minloc (a2, 1, l3, kind=8) .ne. 1)) stop 22
  if (any (minloc (a2, 1, l3, 4, .false.) .ne. 1)) stop 23
  if (any (minloc (a2, 1, l3, 2, .true.) .ne. 10)) stop 24
  b2 = 0
  if (any (minloc (b2) .ne. 1)) stop 1
  if (any (minloc (b2, back=.false.) .ne. 1)) stop 2
  if (any (minloc (b2, back=.true.) .ne. 10)) stop 3
  if (any (minloc (b2, kind=2) .ne. 1)) stop 4
  if (any (minloc (b2, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (minloc (b2, kind=8, back=.true.) .ne. 10)) stop 6
  if (minloc (b2, 1) .ne. 1) stop 7
  if (minloc (b2, 1, back=.false.) .ne. 1) stop 8
  if (minloc (b2, 1, back=.true.) .ne. 10) stop 9
  if (minloc (b2, 1, kind=1) .ne. 1) stop 10
  if (minloc (b2, 1, kind=2, back=.false.) .ne. 1) stop 11
  if (minloc (b2, 1, kind=4, back=.true.) .ne. 10) stop 12
  if (minloc (b2, 1, l2) .ne. 1) stop 13
  if (minloc (b2, 1, l2, back=.false.) .ne. 1) stop 14
  if (minloc (b2, 1, l2, back=.true.) .ne. 10) stop 15
  if (minloc (b2, 1, l2, kind=8) .ne. 1) stop 16
  if (minloc (b2, 1, l2, 4, .false.) .ne. 1) stop 17
  if (minloc (b2, 1, l2, 2, .true.) .ne. 10) stop 18
  if (minloc (b2, 1, l3) .ne. 1) stop 19
  if (minloc (b2, 1, l3, back=.false.) .ne. 1) stop 20
  if (minloc (b2, 1, l3, back=.true.) .ne. 10) stop 21
  if (minloc (b2, 1, l3, kind=8) .ne. 1) stop 22
  if (minloc (b2, 1, l3, 4, .false.) .ne. 1) stop 23
  if (minloc (b2, 1, l3, 2, .true.) .ne. 10) stop 24
  a4 = 0
  if (any (minloc (a4) .ne. 1)) stop 1
  if (any (minloc (a4, back=.false.) .ne. 1)) stop 2
  if (any (minloc (a4, back=.true.) .ne. 10)) stop 3
  if (any (minloc (a4, kind=2) .ne. 1)) stop 4
  if (any (minloc (a4, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (minloc (a4, kind=8, back=.true.) .ne. 10)) stop 6
  if (any (minloc (a4, 1) .ne. 1)) stop 7
  if (any (minloc (a4, 1, back=.false.) .ne. 1)) stop 8
  if (any (minloc (a4, 1, back=.true.) .ne. 10)) stop 9
  if (any (minloc (a4, 1, kind=1) .ne. 1)) stop 10
  if (any (minloc (a4, 1, kind=2, back=.false.) .ne. 1)) stop 11
  if (any (minloc (a4, 1, kind=4, back=.true.) .ne. 10)) stop 12
  if (any (minloc (a4, 1, l1) .ne. 1)) stop 13
  if (any (minloc (a4, 1, l1, back=.false.) .ne. 1)) stop 14
  if (any (minloc (a4, 1, l1, back=.true.) .ne. 10)) stop 15
  if (any (minloc (a4, 1, l1, kind=8) .ne. 1)) stop 16
  if (any (minloc (a4, 1, l1, 4, .false.) .ne. 1)) stop 17
  if (any (minloc (a4, 1, l1, 2, .true.) .ne. 10)) stop 18
  if (any (minloc (a4, 1, l3) .ne. 1)) stop 19
  if (any (minloc (a4, 1, l3, back=.false.) .ne. 1)) stop 20
  if (any (minloc (a4, 1, l3, back=.true.) .ne. 10)) stop 21
  if (any (minloc (a4, 1, l3, kind=8) .ne. 1)) stop 22
  if (any (minloc (a4, 1, l3, 4, .false.) .ne. 1)) stop 23
  if (any (minloc (a4, 1, l3, 2, .true.) .ne. 10)) stop 24
  b4 = 0
  if (any (minloc (b4) .ne. 1)) stop 1
  if (any (minloc (b4, back=.false.) .ne. 1)) stop 2
  if (any (minloc (b4, back=.true.) .ne. 10)) stop 3
  if (any (minloc (b4, kind=2) .ne. 1)) stop 4
  if (any (minloc (b4, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (minloc (b4, kind=8, back=.true.) .ne. 10)) stop 6
  if (minloc (b4, 1) .ne. 1) stop 7
  if (minloc (b4, 1, back=.false.) .ne. 1) stop 8
  if (minloc (b4, 1, back=.true.) .ne. 10) stop 9
  if (minloc (b4, 1, kind=1) .ne. 1) stop 10
  if (minloc (b4, 1, kind=2, back=.false.) .ne. 1) stop 11
  if (minloc (b4, 1, kind=4, back=.true.) .ne. 10) stop 12
  if (minloc (b4, 1, l2) .ne. 1) stop 13
  if (minloc (b4, 1, l2, back=.false.) .ne. 1) stop 14
  if (minloc (b4, 1, l2, back=.true.) .ne. 10) stop 15
  if (minloc (b4, 1, l2, kind=8) .ne. 1) stop 16
  if (minloc (b4, 1, l2, 4, .false.) .ne. 1) stop 17
  if (minloc (b4, 1, l2, 2, .true.) .ne. 10) stop 18
  if (minloc (b4, 1, l3) .ne. 1) stop 19
  if (minloc (b4, 1, l3, back=.false.) .ne. 1) stop 20
  if (minloc (b4, 1, l3, back=.true.) .ne. 10) stop 21
  if (minloc (b4, 1, l3, kind=8) .ne. 1) stop 22
  if (minloc (b4, 1, l3, 4, .false.) .ne. 1) stop 23
  if (minloc (b4, 1, l3, 2, .true.) .ne. 10) stop 24
  a8 = 0
  if (any (minloc (a8) .ne. 1)) stop 1
  if (any (minloc (a8, back=.false.) .ne. 1)) stop 2
  if (any (minloc (a8, back=.true.) .ne. 10)) stop 3
  if (any (minloc (a8, kind=2) .ne. 1)) stop 4
  if (any (minloc (a8, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (minloc (a8, kind=8, back=.true.) .ne. 10)) stop 6
  if (any (minloc (a8, 1) .ne. 1)) stop 7
  if (any (minloc (a8, 1, back=.false.) .ne. 1)) stop 8
  if (any (minloc (a8, 1, back=.true.) .ne. 10)) stop 9
  if (any (minloc (a8, 1, kind=1) .ne. 1)) stop 10
  if (any (minloc (a8, 1, kind=2, back=.false.) .ne. 1)) stop 11
  if (any (minloc (a8, 1, kind=4, back=.true.) .ne. 10)) stop 12
  if (any (minloc (a8, 1, l1) .ne. 1)) stop 13
  if (any (minloc (a8, 1, l1, back=.false.) .ne. 1)) stop 14
  if (any (minloc (a8, 1, l1, back=.true.) .ne. 10)) stop 15
  if (any (minloc (a8, 1, l1, kind=8) .ne. 1)) stop 16
  if (any (minloc (a8, 1, l1, 4, .false.) .ne. 1)) stop 17
  if (any (minloc (a8, 1, l1, 2, .true.) .ne. 10)) stop 18
  if (any (minloc (a8, 1, l3) .ne. 1)) stop 19
  if (any (minloc (a8, 1, l3, back=.false.) .ne. 1)) stop 20
  if (any (minloc (a8, 1, l3, back=.true.) .ne. 10)) stop 21
  if (any (minloc (a8, 1, l3, kind=8) .ne. 1)) stop 22
  if (any (minloc (a8, 1, l3, 4, .false.) .ne. 1)) stop 23
  if (any (minloc (a8, 1, l3, 2, .true.) .ne. 10)) stop 24
  b8 = 0
  if (any (minloc (b8) .ne. 1)) stop 1
  if (any (minloc (b8, back=.false.) .ne. 1)) stop 2
  if (any (minloc (b8, back=.true.) .ne. 10)) stop 3
  if (any (minloc (b8, kind=2) .ne. 1)) stop 4
  if (any (minloc (b8, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (minloc (b8, kind=8, back=.true.) .ne. 10)) stop 6
  if (minloc (b8, 1) .ne. 1) stop 7
  if (minloc (b8, 1, back=.false.) .ne. 1) stop 8
  if (minloc (b8, 1, back=.true.) .ne. 10) stop 9
  if (minloc (b8, 1, kind=1) .ne. 1) stop 10
  if (minloc (b8, 1, kind=2, back=.false.) .ne. 1) stop 11
  if (minloc (b8, 1, kind=4, back=.true.) .ne. 10) stop 12
  if (minloc (b8, 1, l2) .ne. 1) stop 13
  if (minloc (b8, 1, l2, back=.false.) .ne. 1) stop 14
  if (minloc (b8, 1, l2, back=.true.) .ne. 10) stop 15
  if (minloc (b8, 1, l2, kind=8) .ne. 1) stop 16
  if (minloc (b8, 1, l2, 4, .false.) .ne. 1) stop 17
  if (minloc (b8, 1, l2, 2, .true.) .ne. 10) stop 18
  if (minloc (b8, 1, l3) .ne. 1) stop 19
  if (minloc (b8, 1, l3, back=.false.) .ne. 1) stop 20
  if (minloc (b8, 1, l3, back=.true.) .ne. 10) stop 21
  if (minloc (b8, 1, l3, kind=8) .ne. 1) stop 22
  if (minloc (b8, 1, l3, 4, .false.) .ne. 1) stop 23
  if (minloc (b8, 1, l3, 2, .true.) .ne. 10) stop 24
  r4 = 0.0
  if (any (minloc (r4) .ne. 1)) stop 1
  if (any (minloc (r4, back=.false.) .ne. 1)) stop 2
  if (any (minloc (r4, back=.true.) .ne. 10)) stop 3
  if (any (minloc (r4, kind=2) .ne. 1)) stop 4
  if (any (minloc (r4, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (minloc (r4, kind=8, back=.true.) .ne. 10)) stop 6
  if (any (minloc (r4, 1) .ne. 1)) stop 7
  if (any (minloc (r4, 1, back=.false.) .ne. 1)) stop 8
  if (any (minloc (r4, 1, back=.true.) .ne. 10)) stop 9
  if (any (minloc (r4, 1, kind=1) .ne. 1)) stop 10
  if (any (minloc (r4, 1, kind=2, back=.false.) .ne. 1)) stop 11
  if (any (minloc (r4, 1, kind=4, back=.true.) .ne. 10)) stop 12
  if (any (minloc (r4, 1, l1) .ne. 1)) stop 13
  if (any (minloc (r4, 1, l1, back=.false.) .ne. 1)) stop 14
  if (any (minloc (r4, 1, l1, back=.true.) .ne. 10)) stop 15
  if (any (minloc (r4, 1, l1, kind=8) .ne. 1)) stop 16
  if (any (minloc (r4, 1, l1, 4, .false.) .ne. 1)) stop 17
  if (any (minloc (r4, 1, l1, 2, .true.) .ne. 10)) stop 18
  if (any (minloc (r4, 1, l3) .ne. 1)) stop 19
  if (any (minloc (r4, 1, l3, back=.false.) .ne. 1)) stop 20
  if (any (minloc (r4, 1, l3, back=.true.) .ne. 10)) stop 21
  if (any (minloc (r4, 1, l3, kind=8) .ne. 1)) stop 22
  if (any (minloc (r4, 1, l3, 4, .false.) .ne. 1)) stop 23
  if (any (minloc (r4, 1, l3, 2, .true.) .ne. 10)) stop 24
  s4 = 0.0
  if (any (minloc (s4) .ne. 1)) stop 1
  if (any (minloc (s4, back=.false.) .ne. 1)) stop 2
  if (any (minloc (s4, back=.true.) .ne. 10)) stop 3
  if (any (minloc (s4, kind=2) .ne. 1)) stop 4
  if (any (minloc (s4, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (minloc (s4, kind=8, back=.true.) .ne. 10)) stop 6
  if (minloc (s4, 1) .ne. 1) stop 7
  if (minloc (s4, 1, back=.false.) .ne. 1) stop 8
  if (minloc (s4, 1, back=.true.) .ne. 10) stop 9
  if (minloc (s4, 1, kind=1) .ne. 1) stop 10
  if (minloc (s4, 1, kind=2, back=.false.) .ne. 1) stop 11
  if (minloc (s4, 1, kind=4, back=.true.) .ne. 10) stop 12
  if (minloc (s4, 1, l2) .ne. 1) stop 13
  if (minloc (s4, 1, l2, back=.false.) .ne. 1) stop 14
  if (minloc (s4, 1, l2, back=.true.) .ne. 10) stop 15
  if (minloc (s4, 1, l2, kind=8) .ne. 1) stop 16
  if (minloc (s4, 1, l2, 4, .false.) .ne. 1) stop 17
  if (minloc (s4, 1, l2, 2, .true.) .ne. 10) stop 18
  if (minloc (s4, 1, l3) .ne. 1) stop 19
  if (minloc (s4, 1, l3, back=.false.) .ne. 1) stop 20
  if (minloc (s4, 1, l3, back=.true.) .ne. 10) stop 21
  if (minloc (s4, 1, l3, kind=8) .ne. 1) stop 22
  if (minloc (s4, 1, l3, 4, .false.) .ne. 1) stop 23
  if (minloc (s4, 1, l3, 2, .true.) .ne. 10) stop 24
  r8 = 0.0
  if (any (minloc (r8) .ne. 1)) stop 1
  if (any (minloc (r8, back=.false.) .ne. 1)) stop 2
  if (any (minloc (r8, back=.true.) .ne. 10)) stop 3
  if (any (minloc (r8, kind=2) .ne. 1)) stop 4
  if (any (minloc (r8, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (minloc (r8, kind=8, back=.true.) .ne. 10)) stop 6
  if (any (minloc (r8, 1) .ne. 1)) stop 7
  if (any (minloc (r8, 1, back=.false.) .ne. 1)) stop 8
  if (any (minloc (r8, 1, back=.true.) .ne. 10)) stop 9
  if (any (minloc (r8, 1, kind=1) .ne. 1)) stop 10
  if (any (minloc (r8, 1, kind=2, back=.false.) .ne. 1)) stop 11
  if (any (minloc (r8, 1, kind=4, back=.true.) .ne. 10)) stop 12
  if (any (minloc (r8, 1, l1) .ne. 1)) stop 13
  if (any (minloc (r8, 1, l1, back=.false.) .ne. 1)) stop 14
  if (any (minloc (r8, 1, l1, back=.true.) .ne. 10)) stop 15
  if (any (minloc (r8, 1, l1, kind=8) .ne. 1)) stop 16
  if (any (minloc (r8, 1, l1, 4, .false.) .ne. 1)) stop 17
  if (any (minloc (r8, 1, l1, 2, .true.) .ne. 10)) stop 18
  if (any (minloc (r8, 1, l3) .ne. 1)) stop 19
  if (any (minloc (r8, 1, l3, back=.false.) .ne. 1)) stop 20
  if (any (minloc (r8, 1, l3, back=.true.) .ne. 10)) stop 21
  if (any (minloc (r8, 1, l3, kind=8) .ne. 1)) stop 22
  if (any (minloc (r8, 1, l3, 4, .false.) .ne. 1)) stop 23
  if (any (minloc (r8, 1, l3, 2, .true.) .ne. 10)) stop 24
  s8 = 0.0
  if (any (minloc (s8) .ne. 1)) stop 1
  if (any (minloc (s8, back=.false.) .ne. 1)) stop 2
  if (any (minloc (s8, back=.true.) .ne. 10)) stop 3
  if (any (minloc (s8, kind=2) .ne. 1)) stop 4
  if (any (minloc (s8, kind=4, back=.false.) .ne. 1)) stop 5
  if (any (minloc (s8, kind=8, back=.true.) .ne. 10)) stop 6
  if (minloc (s8, 1) .ne. 1) stop 7
  if (minloc (s8, 1, back=.false.) .ne. 1) stop 8
  if (minloc (s8, 1, back=.true.) .ne. 10) stop 9
  if (minloc (s8, 1, kind=1) .ne. 1) stop 10
  if (minloc (s8, 1, kind=2, back=.false.) .ne. 1) stop 11
  if (minloc (s8, 1, kind=4, back=.true.) .ne. 10) stop 12
  if (minloc (s8, 1, l2) .ne. 1) stop 13
  if (minloc (s8, 1, l2, back=.false.) .ne. 1) stop 14
  if (minloc (s8, 1, l2, back=.true.) .ne. 10) stop 15
  if (minloc (s8, 1, l2, kind=8) .ne. 1) stop 16
  if (minloc (s8, 1, l2, 4, .false.) .ne. 1) stop 17
  if (minloc (s8, 1, l2, 2, .true.) .ne. 10) stop 18
  if (minloc (s8, 1, l3) .ne. 1) stop 19
  if (minloc (s8, 1, l3, back=.false.) .ne. 1) stop 20
  if (minloc (s8, 1, l3, back=.true.) .ne. 10) stop 21
  if (minloc (s8, 1, l3, kind=8) .ne. 1) stop 22
  if (minloc (s8, 1, l3, 4, .false.) .ne. 1) stop 23
  if (minloc (s8, 1, l3, 2, .true.) .ne. 10) stop 24
end
