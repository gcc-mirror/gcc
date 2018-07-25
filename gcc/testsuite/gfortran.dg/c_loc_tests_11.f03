! { dg-do compile }
! { dg-options "-std=f2008" }
!
! Test argument checking for C_LOC with subcomponent parameters.
module c_vhandle_mod
  use iso_c_binding
  
  type double_vector_item
    real(kind(1.d0)), allocatable :: v(:)
  end type double_vector_item
  type(double_vector_item), allocatable, target :: dbv_pool(:)
  real(kind(1.d0)), allocatable, target :: vv(:)

  type foo
     integer :: i
  end type foo
  type foo_item
     type(foo), pointer  :: v => null()
  end type foo_item
  type(foo_item), allocatable :: foo_pool(:)

  type foo_item2
     type(foo), pointer  :: v(:) => null()
  end type foo_item2
  type(foo_item2), allocatable :: foo_pool2(:)


contains 

  type(c_ptr) function get_double_vector_address(handle)
    integer(c_int), intent(in) :: handle
    
    if (.true.) then   ! The ultimate component is an allocatable target 
      get_double_vector_address = c_loc(dbv_pool(handle)%v)  ! OK: Interop type and allocatable
    else
      get_double_vector_address = c_loc(vv)  ! OK: Interop type and allocatable
    endif
    
  end function get_double_vector_address


  type(c_ptr) function get_foo_address(handle)
    integer(c_int), intent(in) :: handle    
    get_foo_address = c_loc(foo_pool(handle)%v)

    get_foo_address = c_loc(foo_pool2(handle)%v) ! { dg-error "Fortran 2018: Noninteroperable array at .1. as argument to C_LOC: Expression is a noninteroperable derived type" }
  end function get_foo_address

    
end module c_vhandle_mod

