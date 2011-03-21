! PR libfortran/47757
! { dg-do run }

  integer(1) :: a1(2,2)
  integer(2) :: a2(2,2)
  integer(4) :: a4(2,2)
  integer(8) :: a8(2,2)
  logical :: mask(2,2)
  logical :: mask2
  a1 = 0
  a2 = 0
  a3 = 0
  a4 = 0
  mask2 = .true.
  mask = reshape([.true.,.true.,.false.,.true.],[2,2])
  print *, iany(a1, dim=1, mask=mask)
  print *, iany(a2, dim=1, mask=mask)
  print *, iany(a4, dim=1, mask=mask)
  print *, iany(a8, dim=1, mask=mask)
  print *, iall(a1, dim=1, mask=mask)
  print *, iall(a2, dim=1, mask=mask)
  print *, iall(a4, dim=1, mask=mask)
  print *, iall(a8, dim=1, mask=mask)
  print *, iparity(a1, dim=1, mask=mask)
  print *, iparity(a2, dim=1, mask=mask)
  print *, iparity(a4, dim=1, mask=mask)
  print *, iparity(a8, dim=1, mask=mask)
  print *, iany(a1, dim=1, mask=mask2)
  print *, iany(a2, dim=1, mask=mask2)
  print *, iany(a4, dim=1, mask=mask2)
  print *, iany(a8, dim=1, mask=mask2)
  print *, iall(a1, dim=1, mask=mask2)
  print *, iall(a2, dim=1, mask=mask2)
  print *, iall(a4, dim=1, mask=mask2)
  print *, iall(a8, dim=1, mask=mask2)
  print *, iparity(a1, dim=1, mask=mask2)
  print *, iparity(a2, dim=1, mask=mask2)
  print *, iparity(a4, dim=1, mask=mask2)
  print *, iparity(a8, dim=1, mask=mask2)
end
