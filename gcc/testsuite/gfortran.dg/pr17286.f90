! { dg-do run }
! PR17286
! Namelist read failed when spaces exist between the '=' and the numbers
! This is a libgfortran bug
! Derived from testcase provided by Paul Thomas <paulthomas2@wanadoo.fr>
       program bug3
       integer num1 , num2 , num3 , num4 
       data num3  / 42 /
       data num4  / 56 /
       namelist /mynml1/ num1,num2
       namelist /mynml2/ num3,num4
       logical dbg
       data dbg / .FALSE. /
       open(unit=10,status='SCRATCH')
       write(10,'(A)') "&mynml1,num1= 16,num2=32,&end"
!
! write mynml2
!
       write(10,mynml2)
       rewind(10)
!
! now read back
!
       num1 = -1
       num2 = -1
       read(10,mynml1)
       if (num1.eq.16.and.num2.eq.32) then
          if (dbg) write(*,mynml1)
       else
          if (dbg) print *, 'expected 16 32 got ',num1,num2
          STOP 1
       endif
       num3 = -1
       num4 = -1
       read(10,mynml2)
       if (num3.eq.42.and.num4.eq.56) then
          if (dbg) write(*,mynml2)
       else
          if (dbg) print *, 'expected 42 56 got ',num3,num4
          STOP 2
       endif

       close(10)
       end
