! PR libfortran/47757
! { dg-do run { target fortran_large_int } }

  integer(16) :: a16(2,2)
  logical :: mask(2,2)
  logical :: mask2
  a16 = 0
  mask2 = .true.
  mask = reshape([.true.,.true.,.false.,.true.],[2,2])
  print *, iany(a16, dim=1, mask=mask)
  print *, iall(a16, dim=1, mask=mask)
  print *, iparity(a16, dim=1, mask=mask)
  print *, iany(a16, dim=1, mask=mask2)
  print *, iall(a16, dim=1, mask=mask2)
  print *, iparity(a16, dim=1, mask=mask2)
end
