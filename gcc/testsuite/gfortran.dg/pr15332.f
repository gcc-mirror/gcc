! PR libfortran/15332
! { dg-do run }
! { dg-options "-std=legacy" }
!
      character*12 c

      write (c,100) 0, 1
      if (c .ne. 'i = 0, j = 1') STOP 1
      
      write (c,100) 0
      if (c .ne. 'i = 0       ') STOP 2

 100  format ('i = ',i1,:,', j = ',i1)
      end
