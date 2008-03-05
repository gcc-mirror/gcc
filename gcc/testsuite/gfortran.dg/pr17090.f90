! { dg-do run { target fd_truncate } }
! pr 17090 Runtime I/O error
! bdavis9659@comcast.net 
! 9/12/2004
! list directed read with spaces between the last data item and the
! eoln cause missed data items.
! this is a libgfortran test case
       implicit none
       integer i,sum
       real a(14)
       data sum / 0 /
       open(unit=9,status='SCRATCH')
       write(9,*)1.0,2.0,3.0,4.0,'      '
       write(9,*)5.0,6.0,7.0,8.0,'      '
       write(9,*)9.0,10.0,11.0,12.0,13.0,14.0,'      '
       rewind(9)
       read(9,*)a
       do i = 1,14
          sum = sum + a(i)
       end do
       if (sum.ne.105) call abort
       end
