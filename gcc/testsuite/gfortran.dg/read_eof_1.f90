! { dg-do run }
! PR25697 Check that reading from a file that is at end-of-file does not
! segfault or give error.  Test case derived from example in PR from Dale Ranta.
! Contributed by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
       integer data(9)
       do i = 1,9
         data(i)=-3
       enddo
       open(unit=11,status='scratch',form='unformatted')
       write(11)data
       read(11,end=        1000 )data
       call abort()
 1000  continue
       backspace 11
       backspace 11
       write(11)data
       rewind 11
       data = 0
       read(11,end=        1001 )data
 1001  continue
       read(11,end=        1002 )data
       call abort
 1002  continue
       if (.not. all(data == -3)) call abort()
       close(11)
       end

