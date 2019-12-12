! { dg-do compile }
!
! PR 85395: [F03] private clause contained in derived type acquires spurious scope
!
! Contributed by <cfd@mnet-mail.de>

module defs
   implicit none

   type :: base
   contains
      private
   end type

   type :: options
      procedure(), pointer, nopass :: ptr
   end type

   type :: t
      private
      procedure(), pointer, nopass, public :: ptr
   end type
end module


program p
   use defs
   implicit none
   type(options) :: self
   type(t) :: dt
   self%ptr => null()
   dt%ptr => null()
end
