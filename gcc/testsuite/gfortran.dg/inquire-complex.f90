! { dg-do run }
! PR 23428:  Inquire(iolength) used to give the wrong result.
program main
  implicit none
  integer s4, s8
  
  complex(kind=8) c8
  complex(kind=4) c4
  
  inquire (iolength=s4) c4
  inquire (iolength=s8) c8
  if (s4 /= 8 .or. s8 /= 16) STOP 1

end program main
