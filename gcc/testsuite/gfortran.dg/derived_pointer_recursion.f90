! { dg-do compile }
! { dg-options "-O0" }
!  Tests patch for PR24092 - This would ICE because of the loop in the
!  derived type definitions.
!
   module llo
      type :: it
         character*10  :: k
         integer :: c(2)
      end type it
      type :: bt
         type (nt), pointer :: p
      end type bt
      type :: nt
         type (it) :: i
         type (bt) :: b
      end type nt
      type (bt), pointer :: ptr
   end module llo
!  copyright 1996 Loren P. Meissner -- May be distributed if this line is included.
!  Linked List operations with Pointer to Pointer

! { dg-final { cleanup-modules "llo" } }
