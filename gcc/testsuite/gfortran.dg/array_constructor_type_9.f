! { dg-do run }
!
! PR fortran/27997
!
! Array constructor with typespec, check for regression
! with fixed form.
!
      integer :: a(2), realabc, real_abc2
      a = [ realabc, real_abc2 ]
      end
