! { dg-do compile }
!
! PR 42188: [OOP] F03:C612. The leftmost part-name shall be the name of a data object
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module grid_module
 implicit none
 type grid
 contains
   procedure :: new_grid
   procedure :: new_int
 end type
contains
 subroutine new_grid(this)
   class(grid) :: this
 end subroutine
 integer function new_int(this)
   class(grid) :: this
   new_int = 42
 end function
end module

module field_module
 use grid_module
 implicit none

 type field
   type(grid) :: mesh
 end type

contains

 type(field) function new_field()
 end function

 subroutine test
   integer :: i
   type(grid) :: g
   g = new_field()%mesh              ! { dg-error "cannot be a function reference" }
   call new_field()%mesh%new_grid()  ! { dg-error "Syntax error" }
   i = new_field() % mesh%new_int()  ! { dg-error "cannot be a function reference" }
 end subroutine

end module
