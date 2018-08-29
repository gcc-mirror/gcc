! { dg-do compile }
!
! PR 82932: [OOP] ICE in update_compcall_arglist, at fortran/resolve.c:5837
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module m

   implicit none

   type, abstract :: AT
   contains
      procedure(init_ifc),    deferred :: sinit
      procedure(missing_ifc), deferred :: missing
      generic :: init    => sinit
   end type

   abstract interface
      subroutine init_ifc(data)
         import AT
         class(AT) :: data
      end subroutine
      subroutine missing_ifc(data)
         import AT
         class(AT) :: data
      end subroutine
   end interface

end module


program p

   use m

   implicit none

   type, extends(AT) :: ET  ! { dg-error "must be ABSTRACT" }
   contains
      procedure :: sinit
   end type

   type(ET) :: c
   call c%init()

end
