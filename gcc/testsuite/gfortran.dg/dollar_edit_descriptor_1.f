! { dg-do run }
! { dg-options "-w" }
! PR libfortran/20006
      character*5 c
      open (42,status='scratch')
      write (42,'(A,$)') 'abc'
      write (42,'(A)') 'de'
      rewind (42)
      read (42,'(A)') c
      close (42)

      if (c /= 'abcde') call abort
      end
