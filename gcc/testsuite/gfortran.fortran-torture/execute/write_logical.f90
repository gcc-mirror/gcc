! PR 14334, L edit descriptor does not work
!
!  this test uses L1 and L4 to print TRUE and FALSE
       logical true,false
       character*10 b
       true = .TRUE.
       false = .FALSE.
       b = ''
       write (b, '(L1)') true
       if (b(1:1) .ne. 'T') STOP 1

       b = ''
       write (b, '(L1)') false
       if (b(1:1) .ne. 'F') STOP 2

       b = ''
       write(b, '(L4)') true
       if (b(1:4) .ne. '   T') STOP 3

       b = ''
       write(b, '(L4)') false
       if (b(1:4) .ne. '   F') STOP 4
       end
