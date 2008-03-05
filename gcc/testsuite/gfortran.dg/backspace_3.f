! { dg-do run { target fd_truncate } }
! PR25598 Error on repeated backspaces.
! Derived from example given in PR by Dale Ranta
! Contributed by Jerry DeLisle  <jvdelisle@gcc.gnu.org> 
       integer data
       data=-1
       open(unit=11,status='scratch',form='unformatted')
       write(11)data
       read(11,end=        1000 )data
       call abort()
 1000  continue
       backspace 11
       backspace 11
       backspace 11
       read(11,end=        1001 )data
 1001  continue
       if (data.ne.-1) call abort
       close(11)
       end

