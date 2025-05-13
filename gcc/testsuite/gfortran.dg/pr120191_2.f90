! PR fortran/120191
! { dg-do run }

  character(kind=1, len=2) :: a(4, 4, 4), b(4)
  logical :: l(4, 4, 4), m, n(4)
  a = 'aa'
  b = 'aa'
  l = .true.
  m = .true.
  n = .true.
  if (any (maxloc (a) .ne. 1)) stop 1
  if (any (maxloc (a, dim=1) .ne. 1)) stop 2
  if (any (maxloc (a, 1) .ne. 1)) stop 3
  if (any (maxloc (a, dim=1, mask=l, kind=4, back=.false.) .ne. 1)) stop 4
  if (any (maxloc (a, 1, l, 4, .false.) .ne. 1)) stop 5
  if (any (maxloc (a, dim=1, mask=m, kind=4, back=.false.) .ne. 1)) stop 6
  if (any (maxloc (a, 1, m, 4, .false.) .ne. 1)) stop 7
  if (any (maxloc (a, dim=1, mask=l, kind=4, back=.true.) .ne. 4)) stop 8
  if (any (maxloc (a, 1, l, 4, .true.) .ne. 4)) stop 9
  if (any (maxloc (a, dim=1, mask=m, kind=4, back=.true.) .ne. 4)) stop 10
  if (any (maxloc (a, 1, m, 4, .true.) .ne. 4)) stop 11
  if (any (maxloc (b) .ne. 1)) stop 12
  if (maxloc (b, dim=1) .ne. 1) stop 13
  if (maxloc (b, 1) .ne. 1) stop 14
  if (maxloc (b, dim=1, mask=n, kind=4, back=.false.) .ne. 1) stop 15
  if (maxloc (b, 1, n, 4, .false.) .ne. 1) stop 16
  if (maxloc (b, dim=1, mask=m, kind=4, back=.false.) .ne. 1) stop 17
  if (maxloc (b, 1, m, 4, .false.) .ne. 1) stop 18
  if (maxloc (b, dim=1, mask=n, kind=4, back=.true.) .ne. 4) stop 19
  if (maxloc (b, 1, n, 4, .true.) .ne. 4) stop 20
  if (maxloc (b, dim=1, mask=m, kind=4, back=.true.) .ne. 4) stop 21
  if (maxloc (b, 1, m, 4, .true.) .ne. 4) stop 22
  l = .false.
  m = .false.
  n = .false.
  if (any (maxloc (a, dim=1, mask=l, kind=4, back=.false.) .ne. 0)) stop 23
  if (any (maxloc (a, 1, l, 4, .false.) .ne. 0)) stop 24
  if (maxloc (b, dim=1, mask=n, kind=4, back=.false.) .ne. 0) stop 25
  if (maxloc (b, 1, n, 4, .false.) .ne. 0) stop 26
  if (maxloc (b, dim=1, mask=m, kind=4, back=.false.) .ne. 0) stop 27
  if (maxloc (b, 1, m, 4, .false.) .ne. 0) stop 28
  if (maxloc (b, dim=1, mask=n, kind=4, back=.true.) .ne. 0) stop 29
  if (maxloc (b, 1, n, 4, .true.) .ne. 0) stop 30
  if (maxloc (b, dim=1, mask=m, kind=4, back=.true.) .ne. 0) stop 31
  if (maxloc (b, 1, m, 4, .true.) .ne. 0) stop 32
  l = .true.
  m = .true.
  n = .true.
  if (any (minloc (a) .ne. 1)) stop 1
  if (any (minloc (a, dim=1) .ne. 1)) stop 2
  if (any (minloc (a, 1) .ne. 1)) stop 3
  if (any (minloc (a, dim=1, mask=l, kind=4, back=.false.) .ne. 1)) stop 4
  if (any (minloc (a, 1, l, 4, .false.) .ne. 1)) stop 5
  if (any (minloc (a, dim=1, mask=m, kind=4, back=.false.) .ne. 1)) stop 6
  if (any (minloc (a, 1, m, 4, .false.) .ne. 1)) stop 7
  if (any (minloc (a, dim=1, mask=l, kind=4, back=.true.) .ne. 4)) stop 8
  if (any (minloc (a, 1, l, 4, .true.) .ne. 4)) stop 9
  if (any (minloc (a, dim=1, mask=m, kind=4, back=.true.) .ne. 4)) stop 10
  if (any (minloc (a, 1, m, 4, .true.) .ne. 4)) stop 11
  if (any (minloc (b) .ne. 1)) stop 12
  if (minloc (b, dim=1) .ne. 1) stop 13
  if (minloc (b, 1) .ne. 1) stop 14
  if (minloc (b, dim=1, mask=n, kind=4, back=.false.) .ne. 1) stop 15
  if (minloc (b, 1, n, 4, .false.) .ne. 1) stop 16
  if (minloc (b, dim=1, mask=m, kind=4, back=.false.) .ne. 1) stop 17
  if (minloc (b, 1, m, 4, .false.) .ne. 1) stop 18
  if (minloc (b, dim=1, mask=n, kind=4, back=.true.) .ne. 4) stop 19
  if (minloc (b, 1, n, 4, .true.) .ne. 4) stop 20
  if (minloc (b, dim=1, mask=m, kind=4, back=.true.) .ne. 4) stop 21
  if (minloc (b, 1, m, 4, .true.) .ne. 4) stop 22
  l = .false.
  m = .false.
  n = .false.
  if (any (minloc (a, dim=1, mask=l, kind=4, back=.false.) .ne. 0)) stop 23
  if (any (minloc (a, 1, l, 4, .false.) .ne. 0)) stop 24
  if (minloc (b, dim=1, mask=n, kind=4, back=.false.) .ne. 0) stop 25
  if (minloc (b, 1, n, 4, .false.) .ne. 0) stop 26
  if (minloc (b, dim=1, mask=m, kind=4, back=.false.) .ne. 0) stop 27
  if (minloc (b, 1, m, 4, .false.) .ne. 0) stop 28
  if (minloc (b, dim=1, mask=n, kind=4, back=.true.) .ne. 0) stop 29
  if (minloc (b, 1, n, 4, .true.) .ne. 0) stop 30
  if (minloc (b, dim=1, mask=m, kind=4, back=.true.) .ne. 0) stop 31
  if (minloc (b, 1, m, 4, .true.) .ne. 0) stop 32
end
