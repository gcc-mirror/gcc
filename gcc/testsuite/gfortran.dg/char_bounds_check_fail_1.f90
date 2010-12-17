! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "Substring out of bounds check" }
! PR fortran/27588
program bound_check
      character*10 zz
      i = 2
      j = i+9
      zz = ' '
      zz(i:j) = 'abcdef'
      print * , zz
      end
! { dg-output "At line 10.*Substring out of bounds: upper bound \\(11\\) of 'zz' exceeds string length" }
