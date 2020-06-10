! { dg-do run }
! PR libfortran/95104 - Segfault on a legal WAIT statement

program test
  wait (10, iostat=ios)
  if (ios /= 0) stop 1
  close (10)
end program test
