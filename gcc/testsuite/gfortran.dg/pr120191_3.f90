! PR fortran/120191
! { dg-do run }

  character(kind=1, len=2) :: a(4, 4, 4), b(4)
  logical :: l(4, 4, 4), m, n(4)
  a = 'aa'
  b = 'aa'
  l = .false.
  m = .false.
  n = .false.
  if (any (maxloc (a, dim=1, mask=m, kind=4, back=.false.) .ne. 0)) stop 1
  if (any (maxloc (a, 1, m, 4, .false.) .ne. 0)) stop 2
  if (any (maxloc (a, dim=1, mask=l, kind=4, back=.true.) .ne. 0)) stop 3
  if (any (maxloc (a, 1, l, 4, .true.) .ne. 0)) stop 4
  if (any (maxloc (a, dim=1, mask=m, kind=4, back=.true.) .ne. 0)) stop 5
  if (any (maxloc (a, 1, m, 4, .true.) .ne. 0)) stop 6
  if (any (minloc (a, dim=1, mask=m, kind=4, back=.false.) .ne. 0)) stop 7
  if (any (minloc (a, 1, m, 4, .false.) .ne. 0)) stop 8
  if (any (minloc (a, dim=1, mask=l, kind=4, back=.true.) .ne. 0)) stop 9
  if (any (minloc (a, 1, l, 4, .true.) .ne. 0)) stop 10
  if (any (minloc (a, dim=1, mask=m, kind=4, back=.true.) .ne. 0)) stop 11
  if (any (minloc (a, 1, m, 4, .true.) .ne. 0)) stop 12
end
