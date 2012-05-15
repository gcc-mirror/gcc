! { dg-do run }
! PR fortran/30793
! Check that pointer-returing functions
! work derived types.
!
! Contributed by Salvatore Filippone.
!
module class_mesh
  type mesh
    real(kind(1.d0)), allocatable :: area(:) 
  end type mesh
contains 
  subroutine create_mesh(msh)
    type(mesh), intent(out) :: msh
    allocate(msh%area(10))
    return
  end subroutine create_mesh
end module class_mesh

module class_field
  use class_mesh
  implicit none
  private ! Default
  public :: create_field, field
  public :: msh_

  type field
     private
     type(mesh),     pointer :: msh   => null()
     integer                 :: isize(2)
  end type field

  interface msh_
    module procedure msh_
  end interface
  interface create_field
    module procedure create_field
  end interface
contains
  subroutine create_field(fld,msh)
    type(field),      intent(out)        :: fld
    type(mesh),       intent(in), target :: msh
    fld%msh => msh
    fld%isize = 1
  end subroutine create_field

  function msh_(fld)
    type(mesh), pointer :: msh_
    type(field), intent(in) :: fld
    msh_ => fld%msh
  end function msh_
end module class_field

module class_scalar_field
  use class_field
  implicit none
  private
  public :: create_field, scalar_field
  public :: msh_

  type scalar_field
    private
    type(field) :: base
    real(kind(1.d0)), allocatable :: x(:)  
    real(kind(1.d0)), allocatable :: bx(:) 
    real(kind(1.d0)), allocatable :: x_old(:) 
  end type scalar_field

  interface create_field
    module procedure create_scalar_field
  end interface
  interface msh_
    module procedure get_scalar_field_msh
  end interface
contains
  subroutine create_scalar_field(fld,msh)
    use class_mesh
    type(scalar_field), intent(out)          :: fld
    type(mesh),         intent(in), target   :: msh
    call create_field(fld%base,msh)
    allocate(fld%x(10),fld%bx(20))
  end subroutine create_scalar_field

  function get_scalar_field_msh(fld)
    use class_mesh
    type(mesh), pointer :: get_scalar_field_msh
    type(scalar_field), intent(in), target  :: fld

    get_scalar_field_msh => msh_(fld%base)
  end function get_scalar_field_msh
end module class_scalar_field

program test_pnt
  use class_mesh
  use class_scalar_field
  implicit none
  type(mesh) :: msh
  type(mesh), pointer  :: mshp
  type(scalar_field) :: quality
  call create_mesh(msh)
  call create_field(quality,msh)
  mshp => msh_(quality)
end program test_pnt
