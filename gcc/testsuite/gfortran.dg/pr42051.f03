! { dg-do compile }
! { dg-options "-fno-whole-file" }
!
! PR fortran/42051
! PR fortran/44064
! Access to freed symbols
!
! Testcase provided by Damian Rouson <damian@rouson.net>,
! reduced by Janus Weil <janus@gcc.gnu.org>.

module grid_module
  implicit none 
  type grid
  end type
  type field
    type(grid) :: mesh
  end type
contains
  real function return_x(this)
    class(grid) :: this
  end function
end module 

module field_module
  use grid_module, only: field,return_x
  implicit none 
contains
  subroutine output(this)
    class(field) :: this
    print *,return_x(this%mesh)
  end subroutine
end module

end

! { dg-final { cleanup-modules "grid_module field_module" } }
