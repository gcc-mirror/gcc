! { dg-do link }
! PR 30415 - minloc and maxloc for integer kinds=1 and 2 were missing
! Test case by Harald Anlauf
program gfcbug55
  integer(kind=1) :: i1(4) = 1
  integer(kind=2) :: i2(4) = 1
  print *, minloc(i1), maxloc(i1)
  print *, minloc(i2), maxloc(i2)
end program gfcbug55

