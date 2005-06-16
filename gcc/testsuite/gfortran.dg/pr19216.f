! PR libfortran/19216
! { dg-do run }
       integer dat(3), i, j
       data dat / 3,2,1 /

       open (20, status='scratch')
       write (20,'(A)') '/ 10 20 30'
       write (20,'(A)') '1 2 3 4'
       write (20,'(A)') '5 6 7 8'
       rewind (20)
       read (20,*) (dat(i), i=1,3)
       if (dat(1).ne.3 .or. dat(2).ne.2 .or. dat(3).ne.1) call abort
       read (20,*) I,J
       if (i .ne. 1 .or. j .ne. 2) call abort
       read (20,*) I,J
       if (i .ne. 5 .or. j .ne. 6) call abort
       close(20)
       end
