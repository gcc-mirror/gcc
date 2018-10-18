! { dg-do run }
!
! PR fortran/87625
!
! Ensure that "var" gets allocated.
!
! Contributed by Tobias Burnus
!
program test
   implicit none
   type t
     integer :: i
   end type t
   class(t), allocatable :: var(:)
   call poly_init()
   print *, var(:)%i
   if (lbound(var, 1) /= 1 .and. ubound(var, 1) /= 2) call abort()
   if (var(1)%i /= 11 .or. var(2)%i /= 12) call abort()
   call poly_init2()
   !print *, var(:)%i
   if (lbound(var, 1) /= 1 .and. ubound(var, 1) /= 3) call abort()
   if (var(1)%i /= 11 .or. var(2)%i /= 12 .or. var(3)%i /= 13) call abort()
contains
   subroutine poly_init()
     !allocate(var(2))
     var = [t :: t(11), t(12)]
   end subroutine poly_init
   subroutine poly_init2()
     var = [t :: t(11), t(12), t(13)]
   end subroutine poly_init2
 end program test
