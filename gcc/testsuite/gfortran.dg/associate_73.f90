!{ dg-do compile }

! Check associate to a "void *" does not ICE.
! Contributed by Matthias Klose  <doko@gcc.gnu.org>
! and Steve Kargl  <kargls@comcast.net>

module pr118789

   implicit none

   CONTAINS

   subroutine fckit_c_nodelete(cptr) bind(c)
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: cptr
      associate( unused_ => cptr )
      end associate
   end subroutine

end module

