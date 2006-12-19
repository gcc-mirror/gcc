! { dg-do run }
!pr18284 BACKSPACE broken
       open(unit=10,access='SEQUENTIAL',status='SCRATCH')
       do I = 1,200
         write(10,*)I
       end do
       backspace(10)
       backspace(10)
       read(10,*)I
       if (I.NE.199) call abort
       end
