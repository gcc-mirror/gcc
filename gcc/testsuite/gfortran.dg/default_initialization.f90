!
! { dg-do compile }
! PR 20845
!
! In ISO/IEC 1539-1:1997(E), 4th constraint in section 11.3:
!
!   If an object of a type for which component-initialization is specified
!   (R429) appears in the specification-part of a module and does not have
!   the ALLOCATABLE or POINTER attribute, the object shall have the SAVE
!   attribute.
!
module bad
   implicit none
   type default_initialization
      integer :: x = 42
   end type default_initialization
   type (default_initialization) t ! { dg-error "default initialization" }
end module bad
