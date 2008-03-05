! { dg-do run { target fd_truncate } }
! PR32446 printing big numbers in F0.1 format.
! This segfaulted before the patch.
      open (10, status="scratch")
      write (10,'(F0.1)') huge(1.0)
      END
