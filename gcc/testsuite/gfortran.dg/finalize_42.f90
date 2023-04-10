! { dg-do run }
!
! Test the fix for PR71798 in which the result of 'create_mytype'
! was not being finalized after the completion of the assignment
! statement.
!
! Contributed by Jonathan Hogg  <jhogg41@gmail.com>
!
module mymod
   implicit none

   integer :: next = 0

   type :: mytype
      integer :: idx = -1
   contains
      procedure :: mytype_assign
      generic :: assignment(=) => mytype_assign
      final :: mytype_final
   end type mytype

contains
   subroutine mytype_assign(this, other)
      class(mytype), intent(inout) :: this
      class(mytype), intent(in) :: other

      this%idx = next
      next = next + 1
   end subroutine mytype_assign

   subroutine mytype_final(this)
      type(mytype) :: this
      next = next + 1
      if (this%idx /= 0) stop 1 ! finalize 'create_mtype' result
   end subroutine mytype_final

   type(mytype) function create_mytype()
      create_mytype%idx = next
      next = next + 1
   end function create_mytype

end module mymod

program test
   use mymod
   implicit none

   type(mytype) :: x

   x = create_mytype()
   if (x%idx /= 1) stop 2       ! Defined assignment failed
   if (next /= 3) stop 3        ! Used to give 2 because finalization did not occur
end program test
