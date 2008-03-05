! { dg-do run { target fd_truncate } }
! PR25835 Check that reading from a file that is at end-of-file does not
! segfault or give error.  Test case derived from example in PR from Dale Ranta.
! Contributed by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
       integer data(2045) ! Exceed internal buffer size
       data=-1
       open(unit=11,status='scratch', form='unformatted')
       write(11)data
       read(11,end=        1000 )data
       call abort()
 1000  continue
       backspace 11
       backspace 11
       data = 0
       read(11)data
       if (.not. all(data == -1)) call abort()
       read(11,end=        1002 )data
       call abort()
 1002  continue
       close(11)
       end
