! Program to test SELECTED_INT_KIND intrinsic function.
Program test_si_kind
  integer*1 i1
  integer*2 i2
  integer*4 i4
  integer*8 i8
  integer res
  real t

  t = huge (i1)
  t = log10 (t)
  res = selected_int_kind (int (t))
  if (res .ne. 1) call abort

  t = huge (i2)
  t = log10 (t)
  res = selected_int_kind (int (t))
  if (res .ne. 2) call abort

  t = huge (i4)
  t = log10 (t)
  res = selected_int_kind (int (t))
  if (res .ne. 4) call abort

  t = huge (i8)
  t = log10 (t)
  res = selected_int_kind (int (t))
  if (res .ne. 8) call abort

  i4 = huge (i4)
  res = selected_int_kind (i4)
  if (res .ne. (-1)) call abort

end program

