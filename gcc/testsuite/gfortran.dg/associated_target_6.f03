! { dg-do run }
! Tests the fix for PR67091 in which the first call to associated
! gave a bad result because the 'target' argument was not being
! correctly handled.
!
! Contributed by 'FortranFan' on clf.
! https://groups.google.com/forum/#!topic/comp.lang.fortran/dN_tQA1Mu-I
!
module m
   implicit none
   private
   type, public :: t
      private
      integer, pointer :: m_i
   contains
      private
      procedure, pass(this), public :: iptr => getptr
      procedure, pass(this), public :: setptr
   end type t
contains
   subroutine setptr( this, iptr )
      !.. Argument list
      class(t), intent(inout)         :: this
      integer, pointer, intent(inout) :: iptr
      this%m_i => iptr
      return
   end subroutine setptr
   function getptr( this ) result( iptr )
      !.. Argument list
      class(t), intent(in) :: this
      !.. Function result
      integer, pointer :: iptr
      iptr => this%m_i
   end function getptr
end module m

program p
   use m, only : t
   integer, pointer :: i
   integer, pointer :: j
   type(t) :: foo
   !.. create i with some value
   allocate (i, source=42)
   call foo%setptr (i)
   if (.not.associated (i, foo%iptr())) STOP 1 ! Gave bad result.
   if (.not.associated (foo%iptr(), i)) STOP 2 ! Was OK.
   j => foo%iptr()
   if (.not.associated (i, j)) STOP 1! Was OK.
end program p
