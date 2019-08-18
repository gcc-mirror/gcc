! pr 15472
! sequential access files
!
!  this test verifies the most basic sequential unformatted I/O
!      write 3 records of various sizes
!      then read them back
!      and compare with what was written
!
       implicit none
       integer size
       parameter(size=100)
       logical debug 
       data debug /.FALSE./
! set debug to true for help in debugging failures.
       integer m(2)
       integer n
       real*4 r(size)
       integer i
       m(1) = int(Z'11111111')
       m(2) = int(Z'22222222')
       n    = int(Z'33333333')
       do i = 1,size
         r(i) = i
       end do
       write(9)m  ! an array of 2
       write(9)n  ! an integer
       write(9)r  ! an array of reals
! zero all the results so we can compare after they are read back
       do i = 1,size
          r(i) = 0
       end do
       m(1) = 0
       m(2) = 0
       n = 0

       rewind(9)
       read(9)m
       read(9)n
       read(9)r
!
! check results
       if (m(1).ne. int(Z'11111111')) then
         if (debug) then
            print '(A,Z8)','m(1) incorrect.  m(1) = ',m(1)
         else
            STOP 1
         endif
       endif

       if (m(2).ne. int(Z'22222222')) then
         if (debug) then
            print '(A,Z8)','m(2) incorrect.  m(2) = ',m(2)
         else
            STOP 2
         endif
       endif

       if (n.ne. int(Z'33333333')) then
         if (debug) then
            print '(A,Z8)','n incorrect.  n = ',n
         else
            STOP 3
         endif
       endif

       do i = 1,size
          if (int(r(i)).ne.i) then
            if (debug) then
              print*,'element ',i,' was ',r(i),' should be ',i
            else
              STOP 4
            endif
          endif
       end do
! use hexdump to look at the file "fort.9"
       if (debug) then
         close(9)
       else
         close(9,status='DELETE')
       endif
       end
