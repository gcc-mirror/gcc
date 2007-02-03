! { dg-do compile }
! PR fortran/30683
! Code contributed by Salvatore Filippone.
!
module class_fld
   integer, parameter :: int_ = 1
  integer, parameter :: bnd_ = 2
  type fld
     integer                 :: size(2)
  end type fld
  !
  !  This interface is renaming the SIZE intrinsic procedure,  
  !  which led to a segmentation fault when trying to resolve
  !  the intrinsic symbol name.
  !
  interface size
     module procedure get_fld_size
  end interface
contains
  function get_fld_size(f)
    integer :: get_fld_size(2)
    type(fld), intent(in) :: f
    get_fld_size(int_) = f%size(int_)
    get_fld_size(bnd_) = f%size(bnd_)
  end function get_fld_size
end module class_fld

module class_s_fld
  use class_fld
  type s_fld
     type(fld) :: base
     real(kind(1.d0)), pointer :: x(:)  => null()
  end type s_fld
  interface x_
     module procedure get_s_fld_x
  end interface
contains
  function get_s_fld_x(fld)
    real(kind(1.d0)), pointer :: get_s_fld_x(:)
    type(s_fld), intent(in) :: fld
    get_s_fld_x => fld%x
  end function get_s_fld_x
end module class_s_fld

module class_s_foo
contains
  subroutine solve_s_foo(phi,var)
    use class_s_fld
    type(s_fld), intent(inout) :: phi
    real(kind(1.d0)), intent(out), optional :: var
    integer :: nsz
    real(kind(1.d0)), pointer :: x(:)
    x => x_(phi)
    nsz=size(x)
  end subroutine solve_s_foo
end module class_s_foo
! { dg-final { cleanup-modules "class_s_fld class_fld class_s_foo" } }
