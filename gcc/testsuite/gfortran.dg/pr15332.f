! PR libfortran/15332
! {do-do run}
      character*12 c

      write (c,100) 0, 1
      if (c .ne. 'i = 0, j = 1') call abort
      
      write (c,100) 0
      if (c .ne. 'i = 0       ') call abort

 100  format ('i = ',i1,:,', j = ',i1)
      end
