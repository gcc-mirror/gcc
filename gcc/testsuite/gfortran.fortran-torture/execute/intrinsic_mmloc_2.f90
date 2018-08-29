program intrinsic_mmloc_2
  real a(-1:1), b(2:3), c(1:2)
  integer, dimension(1):: i
  real (kind = 8), dimension(-1:1) :: vc

  a = 0
  b = 0
  c = 0
  a(-1) = 1
  b(2) = 1
  c(1) = 1

  if (maxloc (a, 1) .ne. 1) STOP 1
  if (maxloc (b, 1) .ne. 1) STOP 2
  if (maxloc (c, 1) .ne. 1) STOP 3


  ! We were giving MINLOC and MAXLOC the wrong return type
  vc = (/4.0d0, 2.50d1, 1.0d1/)
  i = minloc (vc)
  if (i(1) .ne. 1) STOP 4
END PROGRAM
