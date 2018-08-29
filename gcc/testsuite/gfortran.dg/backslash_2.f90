! { dg-do run }
! { dg-options "-fbackslash" }
      integer :: i, e
      open (10, status='scratch')
      write (10,'(A)') '1\n2'
      rewind (10)
      read (10,*,iostat=e) i
      if (e /= 0 .or. i /= 1) STOP 1
      read (10,*,iostat=e) i
      if (e /= 0 .or. i /= 2) STOP 2
      end
