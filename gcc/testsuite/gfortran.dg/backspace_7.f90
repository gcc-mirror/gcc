! { dg-do run { target fd_truncate } }
!pr18284 BACKSPACE broken
       open(unit=10,access='SEQUENTIAL',status='SCRATCH')
       do I = 1,200
         write(10,*)I
       end do
       backspace(10)
       backspace(10)
       read(10,*)I
       if (I.NE.199) STOP 1
       end
