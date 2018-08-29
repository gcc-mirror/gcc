! { dg-do compile }
!
! Part of PR 51946, but breaks easily, therefore introduce its own test
! Authors: Damian Rouson  <damian@sourceryinstitute.org>,
!          Dominique Pelletier  <dominique.pelletier@polymtl.ca>
! Contributed by: Andre Vehreschild  <vehre@gcc.gnu.org>

module integrable_model_module

   implicit none

   type, abstract, public :: integrable_model
      contains
         procedure(default_constructor), deferred :: empty_instance
   end type

   abstract interface
      function default_constructor(this) result(blank_slate)
         import :: integrable_model
         class(integrable_model), intent(in)  :: this
         class(integrable_model), allocatable :: blank_slate
      end function
   end interface

   contains

      subroutine integrate(this)
         class(integrable_model), intent(inout) :: this
         class(integrable_model), allocatable   :: residual
         allocate(residual, source=this%empty_instance())
      end subroutine

end module integrable_model_module
