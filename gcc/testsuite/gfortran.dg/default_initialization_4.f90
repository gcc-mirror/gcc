!
! { dg-do run }
!
! PR fortran/43185
!
! The following is valid F2008 but not valid Fortran 90/2003
! Cf. PR 20845
!
module good
   implicit none
   type default_initialization
      integer :: x = 42
   end type default_initialization
   type (default_initialization) t ! OK in F2008
end module good

use good
if (t%x /= 42) call abort()
t%x = 0
if (t%x /= 0) call abort()
end
! { dg-final { cleanup-modules "good" } }
