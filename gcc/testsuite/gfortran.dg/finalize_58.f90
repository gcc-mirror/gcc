! { dg-do run }
!
! Test fix for PR116388 in which an artificial variable in the finalization
! wrapper was generating an invalid finalization.
!
! Contributed by Tomas Trnka  <trnka@scm.com>
!
module FinalizerTestModule

   use, intrinsic :: ISO_C_BINDING

   implicit none

   type, public :: AType
      type(C_ptr) :: cptr = C_null_ptr
      logical     :: cptr_invalid = .true.
      integer, allocatable :: x(:)
   contains
      final              :: FinalizerA
   end type

   type, public :: BType
      type(C_ptr) :: cptr = C_null_ptr
      type(AType) :: a
   contains
      procedure, public  :: New => NewB
      final              :: FinalizerB
   end type

   type, public :: CType
      type(BType) :: b
   contains
      procedure, public :: New => NewC
   end type

   integer :: final_A = 0
   integer :: final_B = 0
contains

   impure elemental subroutine FinalizerA(self)
      type(AType), intent(inout) :: self
      final_A = final_A + 1
      if (.not. self%cptr_invalid) stop 1
   end subroutine

   subroutine NewB(self)
      class(BType),     intent(out)           :: self

   end subroutine

   impure elemental subroutine FinalizerB(self)
      type(BType), intent(inout) :: self
      final_B = final_B + 1
      if (transfer (self%cptr, C_LONG_LONG) /= 0) stop 2
   end subroutine

   subroutine NewC(self, b)
      class(CType),  intent(out) :: self
      type(BType),   intent(in)  :: b

      self%b = b
   end subroutine

end module

program finalizing_uninitialized
   use FinalizerTestModule
   implicit none

   type(BType) :: b
   type(CType) :: c

   call b%New()
   call c%New(b)
   if (final_A /= 3) stop 3
   if (final_B /= 3) stop 4
end program
