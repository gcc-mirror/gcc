! Check we do the right thing with extreme values.
! From PR12704
program intrinsic_mmloc_3
  integer, dimension(2) :: d
  integer, dimension(2,2) :: a

  d = -huge (d)
  if (maxloc (d, 1) .ne. 1) call abort()
  a = huge (a)
  d = minloc (a)
  if (any (d .ne. 1)) call abort()
end program
