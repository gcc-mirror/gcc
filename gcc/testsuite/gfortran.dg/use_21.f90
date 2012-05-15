! { dg-do compile }
! { dg-options "-Wall" }
!
! PR fortran/51056
!
! Contributed by Kacper Kowalik
!
module domain
   implicit none
   private
   public :: domain_container, dom

   type :: domain_container
      integer :: D_x      !< set to 1 when x-direction exists, 0 otherwise
    contains
      procedure :: init => init_domain_container
   end type domain_container

   type(domain_container) :: dom

   contains
      subroutine init_domain_container(this)
         implicit none
         class(domain_container), intent(inout) :: this
         this%D_x = 0
      end subroutine init_domain_container
end module domain

program ala
   use domain, only: dom
   implicit none
   call dom%init
end program ala
