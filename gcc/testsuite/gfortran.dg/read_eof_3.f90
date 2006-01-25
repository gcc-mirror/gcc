! { dg-do run }
! PR25835 Check that reading from a file that is at end-of-file does not
! segfault or give error.  Test case derived from example in PR from Dale Ranta.
! Contributed by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
       integer data(5000)
       data=-256
       open(unit=11,status='scratch', form='unformatted')
       write(11)data
       write(11)data
       read(11,end=        1000 )data
       call abort()
 1000  continue
       backspace 11
       rewind 11
       write(11)data
       read(11,end=        1001 )data
       call abort()
 1001  continue
       data = 0
       backspace 11
       rewind 11
       read(11,end=        1002 )data
       if (.not. all(data == -256)) call abort()
 1002  continue
       read(11,end=        1003 )data
       call abort()
 1003  continue
       close(11)
       end


