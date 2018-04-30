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
  if (res .ne. 4) STOP 1

  res = selected_real_kind (r = i8)
  if (res .ne. 8) STOP 2

! We can in fact have kinds wider than r8.  How do we want to check?
! res = selected_real_kind (r = (i8 + 1))
! if (res .ne. -2) STOP 3

  res = selected_real_kind (p = precision (r4))
  if (res .ne. 4) STOP 4

  res = selected_real_kind (p = precision (r4), r = i4)
  if (res .ne. 4) STOP 5

  res = selected_real_kind (p = precision (r4), r = i8)
  if (res .ne. 8) STOP 6

! res = selected_real_kind (p = precision (r4), r = i8 + 1)
! if (res .ne. -2) STOP 7

  res = selected_real_kind (p = precision (r8))
  if (res .ne. 8) STOP 8

  res = selected_real_kind (p = precision (r8), r = i4)
  if (res .ne. 8) STOP 9

  res = selected_real_kind (p = precision (r8), r = i8)
  if (res .ne. 8) STOP 10

! res = selected_real_kind (p = precision (r8), r = i8 + 1)
! if (res .ne. -2) STOP 11

! res = selected_real_kind (p = (precision (r8) + 1))
! if (res .ne. -1) STOP 12

! res = selected_real_kind (p = (precision (r8) + 1), r = i4)
! if (res .ne. -1) STOP 13

! res = selected_real_kind (p = (precision (r8) + 1), r = i8)
! if (res .ne. -1) STOP 14

! res = selected_real_kind (p = (precision (r8) + 1), r = i8 + 1)
! if (res .ne. -3) STOP 15

end

