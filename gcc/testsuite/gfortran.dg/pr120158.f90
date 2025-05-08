! PR libfortran/120158
! { dg-do run { target fortran_large_int } }
! { dg-additional-options "-funsigned" }

  unsigned(kind=8) :: a(10, 10, 10), b(10, 10)
  integer(kind=8) :: c(10, 10), d(10, 10)
  a = 0u_8
  if (maxval (a) .ne. 0u_8) stop 1
  b = maxval (a, 1)
  if (any (b .ne. 0u_8)) stop 2
  c = maxloc (a, 1)
  d = maxloc (a, 2, back=.true.)
  if (any (c .ne. 1)) stop 3
  if (any (d .ne. 10)) stop 4
end
