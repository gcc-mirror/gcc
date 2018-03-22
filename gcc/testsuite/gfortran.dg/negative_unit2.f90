! { dg-do run }
! Test case submitted by Dominique d'Humieres
program negative_unit2
  integer :: i, j
  ! i should be <= NEWUNIT_FIRST in libgfortran/io/unit.c
  i = -100
  write(unit=i,fmt=*, iostat=j) 10
  if (j == 0) STOP 1
end program negative_unit2
