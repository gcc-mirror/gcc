! { dg-do run }
!
! PR libfortran/19155
! We accept 'E+00' as a valid real number. The standard says it is not,
! but doesn't require us to issue an error. Since g77 accepts this as zero,
! we do the same.
      real a
      character*10 c
      a = 42
      open (19,status='scratch')
      write (19,'(A15)') 'E+00'
      rewind (19)
      read (19,'(E15.8)') a
      if (a .ne. 0) call abort
      close (19)

      c = "+         "
      read (c,"(F10.4)") a
      if (a /= 0) call abort
      end
