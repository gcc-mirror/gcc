! PR libfortran/15332 and PR fortran/13257
! We used to accept this as an extension but
! did do the correct thing at runtime.
! Note the missing , before i1 in the format.
! { dg-do run }
! { dg-options "" }
      character*12 c

      write (c,100) 0, 1
      if (c .ne. 'i = 0, j = 1') STOP 1
      
      write (c,100) 0
      if (c .ne. 'i = 0       ') STOP 2

 100  format ('i = 'i1,:,', j = ',i1)
      end
