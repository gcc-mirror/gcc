! { dg-do run }
! { dg-options "-fno-backslash" }
      character(len=4) a
      open (10, status='scratch')
      write (10,'(A)') '1\n2'
      rewind (10)
      read (10,'(A)') a
      if (a /= '1\n2') call abort
      end
