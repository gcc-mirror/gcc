! PR fortran/13257
! Note the missing , after i4 in the format.
! { dg-do run }
! { dg-options "-std=legacy" }
      character*6 c
      write (c,1001) 1
      if (c .ne. '    1 ') STOP 1

 1001 format (' ',i4' ')
      end
