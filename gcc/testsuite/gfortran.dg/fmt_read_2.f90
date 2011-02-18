! { dg-do compile }
! PR fortran/32483
      implicit none
      integer :: r
      real :: a
      write (*,'(i0)') r
      read (*,'(i0)') r ! { dg-error "Positive width required" }
      read (*,'(f0.2)') a ! { dg-error "Positive width required" }
      print *, r,a
      END
