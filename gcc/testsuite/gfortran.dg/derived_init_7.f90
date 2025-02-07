! { dg-do run }
! Check that finalizable intent(out) dummy arguments are first finalized
! and then correctly default-initialized (PR116829)
!
! Contributed by Tomas Trnka  <trnka@scm.com>
!
module FinalizableIntentOutTestModule
   implicit none

   type :: AapType
      integer  :: i = 0
   contains
      final    :: Finalizer
   end type
   integer :: ctr = 0
   logical :: err1 = .false.
   logical :: err2 = .false.
contains

   subroutine Finalizer(self)
      type(AapType), intent(inout) :: self

      ! Fail if Finalizer gets called again on an already finalized object
      if (self%i == 42) err1 = .true.

      self%i = 42 ! Nobody should ever see this value after finalization
      ctr = ctr + 1
   end subroutine

end module


program test
   use FinalizableIntentOutTestModule

   implicit none

   type(AapType) :: aap

   ! Set "i" to nonzero so that initialization in MakeAap has something to do
   aap%i = 1

   call MakeAap(aap)
   
   if (err1) stop 1
   if (err2) stop 2      ! This was failing
   if (ctr /= 1) stop 3  ! Belt and braces to ensure number of final calls correct.

contains

   subroutine MakeAap(a)
      type(AapType), intent(out) :: a

      ! Fail if "a" wasn't initialized properly
      if (a%i /= 0)  err2 = .true.
   end subroutine

end program
