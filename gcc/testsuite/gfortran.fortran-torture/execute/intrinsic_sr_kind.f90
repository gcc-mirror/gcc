! Program to test SELECTED_REAL_KIND intrinsic function.
Program test_sr_kind
  integer res, i4, i8, t
  real*4 r4
  real*8 r8

  i4 = int (log10 (huge (r4)))
  t = - int (log10 (tiny (r4)))
  if (i4 .gt. t) i4 = t

  i8 = int (log10 (huge (r8)))
  t = - int (log10 (tiny (r8)))
  if (i8 .gt. t) i8 = t

  res = selected_real_kind (r = i4)
  if (res .ne. 4) call abort

  res = selected_real_kind (r = i8)
  if (res .ne. 8) call abort

! We can in fact have kinds wider than r8.  How do we want to check?
! res = selected_real_kind (r = (i8 + 1))
! if (res .ne. -2) call abort

  res = selected_real_kind (p = precision (r4))
  if (res .ne. 4) call abort

  res = selected_real_kind (p = precision (r4), r = i4)
  if (res .ne. 4) call abort

  res = selected_real_kind (p = precision (r4), r = i8)
  if (res .ne. 8) call abort

! res = selected_real_kind (p = precision (r4), r = i8 + 1)
! if (res .ne. -2) call abort

  res = selected_real_kind (p = precision (r8))
  if (res .ne. 8) call abort

  res = selected_real_kind (p = precision (r8), r = i4)
  if (res .ne. 8) call abort

  res = selected_real_kind (p = precision (r8), r = i8)
  if (res .ne. 8) call abort

! res = selected_real_kind (p = precision (r8), r = i8 + 1)
! if (res .ne. -2) call abort

! res = selected_real_kind (p = (precision (r8) + 1))
! if (res .ne. -1) call abort

! res = selected_real_kind (p = (precision (r8) + 1), r = i4)
! if (res .ne. -1) call abort

! res = selected_real_kind (p = (precision (r8) + 1), r = i8)
! if (res .ne. -1) call abort

! res = selected_real_kind (p = (precision (r8) + 1), r = i8 + 1)
! if (res .ne. -3) call abort

end

