*     Fixed by 1998-09-28 libI77/open.c change.
      open(90,status='scratch')
      write(90, '(1X, I1 / 1X, I1)') 1, 2
      rewind 90
      write(90, '(1X, I1)') 1
      rewind 90                 ! implicit ENDFILE expected
      read(90, *) i
      read(90, *, end=10) j
      call abort()
 10   end
