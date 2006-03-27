! Check we do the right thing with extreme values.
! From PR12704
program intrinsic_mmloc_3
  integer, dimension(2) :: d
  integer, dimension(2,2) :: a
  logical, dimension(2) :: k
  logical, dimension(2,2) :: l

  k = .true.
  l = .true.

  d = -huge (d)
  if (maxloc (d, 1) .ne. 1) call abort ()

  d = huge (d)
  if (minloc (d, 1) .ne. 1) call abort ()

  d = -huge (d)
  if (maxloc (d, 1, k) .ne. 1) call abort ()

  d = huge (d)
  if (minloc (d, 1, k) .ne. 1) call abort ()

  a = -huge (a)
  d = maxloc (a)
  if (any (d .ne. 1)) call abort ()

  a = huge (a)
  d = minloc (a)
  if (any (d .ne. 1)) call abort ()

  a = -huge (a)
  d = maxloc (a, l)
  if (any (d .ne. 1)) call abort ()

  a = huge (a)
  d = minloc (a, l)
  if (any (d .ne. 1)) call abort ()

end program
