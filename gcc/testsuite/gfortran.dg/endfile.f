! { dg-do run { target fd_truncate } }
! PR25550 file data corrupted after reading end of file.
! Derived from example given in PR from Dale Ranta.
! Contributed by Jerry DeLisle  <jvdelisle@gcc.gnu.org> 
      integer data
      data=-1
      open(unit=11,status='scratch',form='unformatted')
      write(11)data
      read(11,end=1000 )data
      call abort()
 1000 continue
      rewind (11)
      read(11)data
 1001 continue
      if(data.ne.-1) call abort
      end


