! PR libfortran/120196
! { dg-do run }

program pr120196
  character(len=:, kind=1), allocatable :: a(:), s
  character(len=:, kind=4), allocatable :: b(:), t
  logical, allocatable :: l(:)
  logical :: m
  allocate (character(len=16, kind=1) :: a(10), s)
  allocate (l(10))
  a(:) = ""
  s = "*"
  l = .true.
  m = .true.
  if (findloc (a, s, dim=1, back=.true.) .ne. 0) stop 1
  if (findloc (a, s, mask=l, dim=1, back=.true.) .ne. 0) stop 2
  if (findloc (a, s, mask=m, dim=1, back=.true.) .ne. 0) stop 3
  deallocate (a, s)
  allocate (character(len=16, kind=4) :: b(10), t)
  b(:) = ""
  t = "*"
  if (findloc (b, t, dim=1, back=.true.) .ne. 0) stop 4
  if (findloc (b, t, mask=l, dim=1, back=.true.) .ne. 0) stop 5
  if (findloc (b, t, mask=m, dim=1, back=.true.) .ne. 0) stop 6
  deallocate (b, t, l)
end program pr120196
