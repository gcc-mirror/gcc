! { dg-do compile }
!
! PR 42048: [F03] Erroneous syntax error message on TBP call
!
! Contributed by Damian Rouson <rouson@sandia.gov>

module grid_module
 implicit none
 type grid
 contains
   procedure :: new_grid
 end type
contains
 subroutine new_grid(this)
   class(grid) :: this
 end subroutine
end module

module field_module
 use grid_module
 implicit none

 type field
   type(grid) :: mesh
 end type

contains

 type(field) function new_field()
  call new_field%mesh%new_grid()
 end function

 function new_field2() result(new)
  type(field) :: new
  call new%mesh%new_grid()
 end function

 type(field) function new_field3()
  call g()
 contains
  subroutine g()
    call new_field3%mesh%new_grid()
  end subroutine g
 end function new_field3

end module

! { dg-final { cleanup-modules "grid_module field_module" } }
