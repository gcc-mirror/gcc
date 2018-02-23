! { dg-do run { target fd_truncate } }
! PR25835 Check that reading from a file that is at end-of-file does not
! segfault or give error.  Test case derived from example in PR from Dale Ranta.
! Contributed by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
       integer data(5000)
       data=-256
       open(unit=11,status='scratch', form='unformatted')
       write(11)data
       write(11)data
       read(11,end=        1000 )data
       STOP 1
 1000  continue
       backspace 11
       rewind 11
       write(11)data
       read(11,end=        1001 )data
       STOP 2
 1001  continue
       data = 0
       backspace 11
       rewind 11
       read(11,end=        1002 )data
       if (.not. all(data == -256)) STOP 3
 1002  continue
       read(11,end=        1003 )data
       STOP 4
 1003  continue
       close(11)
       end


