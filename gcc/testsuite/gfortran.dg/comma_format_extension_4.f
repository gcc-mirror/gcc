! PR fortran/13257
! Note the missing , before i1 in the format.
! { dg-do run }
! { dg-options "" }
      character*6 c
      write (c,1001) 1
      if (c .ne. '    1 ') call abort

 1001 format (' ',i4' ')
      end
