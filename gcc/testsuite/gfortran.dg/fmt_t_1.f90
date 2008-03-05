! { dg-do run { target fd_truncate } }
      integer nrow, vec(15)
      open (10, status="scratch")
      write (10, fmt='(a)') '001    1 2 3 4 5 6'
      write (10, fmt='(a)') '000000 7 8 9101112'
      write (10, fmt='(a)') '000000131415'
      rewind (10)
      read (10, fmt='(i6, (t7, 6i2))') nrow, (vec(i), i=1,15)
      close (10)
      if (nrow.ne.1) call abort
      if (any (vec.ne.(/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15/))) call abort
      end
