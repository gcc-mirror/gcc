! PR fortran/13257
! Note the missing , before i1 in the format.
! { do-do run }
! { dg-options "" }
      character*5 c
      write (c,1001) 1
      if (c .ne. '    1 ') call abort

 1001 format (' ',i4' ')
      end
