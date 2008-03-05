! { dg-do run { target fd_truncate } }
! { dg-options "-fbackslash" }
      integer :: i, e
      open (10, status='scratch')
      write (10,'(A)') '1\n2'
      rewind (10)
      read (10,*,iostat=e) i
      if (e /= 0 .or. i /= 1) call abort
      read (10,*,iostat=e) i
      if (e /= 0 .or. i /= 2) call abort
      end
