! { dg-do run }
!
! Test the fix for PR40737 comment 17 as part of the overall fix for PR34640.
!
! Contributed by Josh Hykes  <joshuahykes@yahoo.com>
!
   module test_mod
!
   type t1
      character(8)  :: string
   end type t1
!
   type t2
     integer :: tab
     type(t1), pointer :: fp(:)
   end type t2
!
   type t3
      integer :: tab
      type(t2), pointer :: as
   end type t3
!
   type(t3), pointer :: as_typ(:) => null()
!
   character(8),  pointer, public :: p(:)
!
   contains
!
   subroutine as_set_alias (i)
!
   implicit none
!
   integer, intent(in)    :: i
!
     allocate (as_typ(2))
     allocate (as_typ(1)%as)
     allocate (as_typ(1)%as%fp(2), source = [t1("abcdefgh"),t1("ijklmnop")])
     p => as_typ(i)%as%fp(:)%string
!
   end subroutine as_set_alias
!
   end module test_mod

   program test_prog
   use test_mod
   call as_set_alias(1)
   if (any (p .ne. ["abcdefgh","ijklmnop"])) call abort
   deallocate (as_typ(1)%as%fp)
   deallocate (as_typ(1)%as)
   deallocate (as_typ)
   end program test_prog
