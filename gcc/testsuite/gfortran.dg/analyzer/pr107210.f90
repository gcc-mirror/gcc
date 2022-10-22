! { dg-additional-options "-O1" }

subroutine check_int (j)
  INTEGER(4) :: i, ia(5), ib(5,4), ip, ipa(:)
  target :: ib
  POINTER :: ip, ipa
  logical :: l(5)

  ipa=>ib(2:3,1)

  l = (/ sizeof(i) == 4, sizeof(ia) == 20, sizeof(ib) == 80, &
       sizeof(ip) == 4, sizeof(ipa) == 8 /)

  if (any(.not.l)) STOP 4

end subroutine check_int
