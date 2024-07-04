! { dg-do compile }
!
! This contains both testcases in the PR
!
! Contributed by Tomas Trnka  <trnka@scm.com>
!
! First testcase
module BogusPointerArgError
   implicit none

   type :: AType
   end type

contains

   subroutine A ()

      class(AType), allocatable :: x

      allocate(x)
      call B (x)                       ! Was an error here
   end subroutine

   subroutine B (y)
      class(AType), intent(in)    :: y
   end subroutine

   subroutine C (z)
      class(AType), intent(in) :: z(:)

      associate (xxx => z(1))
      end associate

   end subroutine

end module

! Second testcase
module AModule
   implicit none
   private

   public AType

   type, abstract :: AType
   contains
      generic, public :: assignment(=) => Assign

      procedure, private :: Assign
   end type AType

contains

   subroutine Assign(lhs, rhs)
      class(AType), intent(inout) :: lhs
      class(AType), intent(in)    :: rhs
   end subroutine

end module AModule



module ICEGetDescriptorField
   use AModule
   implicit none

contains

   subroutine Foo (x)
      class(AType), intent(in)    :: x(:)

      class(AType), allocatable :: y

      associate (xxx => x(1))
         y = xxx                       ! Was an ICE here
      end associate
   end subroutine

end module ICEGetDescriptorField
