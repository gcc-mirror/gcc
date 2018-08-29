! { dg-do run }
! PR25598 Error on repeated backspaces.
! Derived from example given in PR by Dale Ranta
! Contributed by Jerry DeLisle  <jvdelisle@gcc.gnu.org> 
       integer data
       data=-1
       open(unit=11,status='scratch',form='unformatted')
       write(11)data
       read(11,end=        1000 )data
       STOP 1
 1000  continue
       backspace 11
       backspace 11
       read(11,end=        1001 )data
 1001  continue
       if (data.ne.-1) STOP 1
       close(11)
       end
