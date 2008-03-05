! { dg-do run { target fd_truncate } }
! PR 34887 - reverse tabs followed by a slash used to confuse I/O.
      program main
      character(len=2) :: b, a
      open(10,form="formatted")
      write (10,'(3X, A, T1, A,/)') 'aa', 'bb'
      rewind(10)
      read (10,'(A2,1X,A2)') b,a
      if (a /= 'aa' .or. b /= 'bb') call abort
      close(10,status="delete")
      end
