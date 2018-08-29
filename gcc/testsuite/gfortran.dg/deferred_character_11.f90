! { dg-do run }
!
! Test the fix for PR61147.
!
! Contributed by Thomas Clune  <Thomas.L.Clune@nasa.gov>
!
module B_mod

   type :: B
      character(:), allocatable :: string
   end type B

contains

   function toPointer(this) result(ptr)
      character(:), pointer :: ptr
      class (B), intent(in), target :: this

         ptr => this%string

   end function toPointer

end module B_mod

program main
   use B_mod

   type (B) :: obj
   character(:), pointer :: p

   obj%string = 'foo'
   p => toPointer(obj)

   If (len (p) .ne. 3) STOP 1
   If (p .ne. "foo") STOP 2

end program main


