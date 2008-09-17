! { dg-do compile }
! Tests the fix for PR37274 a regression in which the derived type,
! 'vector' of the function results contained in 'class_motion' is
! private and is incorrectly detected to be ambiguous in 'smooth_mesh'.
!
! Contributed by Salvatore Filippone  <sfilippone@uniroma2.it>
!
module class_vector

  implicit none

  private ! Default
  public :: vector                                  
  public :: vector_ 

  type vector
     private
     real(kind(1.d0)) :: x
     real(kind(1.d0)) :: y
     real(kind(1.d0)) :: z
  end type vector

contains
  ! ----- Constructors -----

  ! Public default constructor
  elemental function vector_(x,y,z)
    type(vector) :: vector_
    real(kind(1.d0)), intent(in) :: x, y, z

    vector_ = vector(x,y,z)

  end function vector_

end module class_vector

module class_dimensions

  implicit none

  private ! Default
  public :: dimensions

  type dimensions
     private
     integer :: l
     integer :: m
     integer :: t
     integer :: theta
  end type dimensions


end module class_dimensions

module tools_math

  implicit none


  interface lin_interp
     function lin_interp_s(f1,f2,fac)
       real(kind(1.d0)) :: lin_interp_s
       real(kind(1.d0)), intent(in) :: f1, f2
       real(kind(1.d0)), intent(in) :: fac
     end function lin_interp_s

     function lin_interp_v(f1,f2,fac)
       use class_vector
       type(vector) :: lin_interp_v
       type(vector),     intent(in) :: f1, f2
       real(kind(1.d0)), intent(in) :: fac
     end function lin_interp_v
  end interface


  interface pwl_deriv
     subroutine pwl_deriv_x_s(dydx,x,y_data,x_data)
       real(kind(1.d0)), intent(out) :: dydx
       real(kind(1.d0)), intent(in) :: x
       real(kind(1.d0)), intent(in) :: y_data(:)
       real(kind(1.d0)), intent(in) :: x_data(:)
     end subroutine pwl_deriv_x_s

     subroutine pwl_deriv_x_v(dydx,x,y_data,x_data)
       real(kind(1.d0)), intent(out) :: dydx(:)
       real(kind(1.d0)), intent(in) :: x
       real(kind(1.d0)), intent(in) :: y_data(:,:)
       real(kind(1.d0)), intent(in) :: x_data(:)
     end subroutine pwl_deriv_x_v

     subroutine pwl_deriv_x_vec(dydx,x,y_data,x_data)
       use class_vector
       type(vector), intent(out) :: dydx
       real(kind(1.d0)), intent(in) :: x
       type(vector), intent(in) :: y_data(:)
       real(kind(1.d0)), intent(in) :: x_data(:)
     end subroutine pwl_deriv_x_vec
  end interface

end module tools_math

module class_motion

  use class_vector
 
  implicit none
  
  private 
  public :: motion 
  public :: get_displacement, get_velocity

  type motion
     private
     integer :: surface_motion
     integer :: vertex_motion
     !
     integer :: iml
     real(kind(1.d0)), allocatable :: law_x(:) 
     type(vector), allocatable :: law_y(:)  
  end type motion

contains


  function get_displacement(mot,x1,x2)
    use tools_math

    type(vector) :: get_displacement
    type(motion), intent(in) :: mot
    real(kind(1.d0)), intent(in) :: x1, x2
    !
    integer :: i1, i2, i3, i4
    type(vector) :: p1, p2, v_A, v_B, v_C, v_D
    type(vector) :: i_trap_1, i_trap_2, i_trap_3

    get_displacement = vector_(0.d0,0.d0,0.d0)
    
  end function get_displacement


  function get_velocity(mot,x)
    use tools_math

    type(vector) :: get_velocity
    type(motion), intent(in) :: mot
    real(kind(1.d0)), intent(in) :: x
    !
    type(vector) :: v
    
    get_velocity = vector_(0.d0,0.d0,0.d0)
    
  end function get_velocity
  
  

end module class_motion

module class_bc_math
  
  implicit none

  private 
  public :: bc_math                           

  type bc_math
     private
     integer :: id
     integer :: nbf
     real(kind(1.d0)), allocatable :: a(:) 
     real(kind(1.d0)), allocatable :: b(:) 
     real(kind(1.d0)), allocatable :: c(:) 
  end type bc_math

  
end module class_bc_math

module class_bc

  use class_bc_math
  use class_motion

  implicit none

  private 
  public :: bc_poly                          
  public :: get_abc, &
       &    get_displacement, get_velocity  

  type bc_poly
     private
     integer :: id
     type(motion) :: mot
     type(bc_math), pointer :: math => null()
  end type bc_poly


  interface get_displacement
     module procedure get_displacement, get_bc_motion_displacement
  end interface

  interface get_velocity
     module procedure get_velocity, get_bc_motion_velocity
  end interface

  interface get_abc
     module procedure get_abc_s, get_abc_v
  end interface
  
contains


  subroutine get_abc_s(bc,dim,id,a,b,c)
    use class_dimensions
    
    type(bc_poly), intent(in) :: bc
    type(dimensions), intent(in) :: dim
    integer, intent(out) :: id
    real(kind(1.d0)), intent(inout) :: a(:)
    real(kind(1.d0)), intent(inout) :: b(:)
    real(kind(1.d0)), intent(inout) :: c(:)
    
    
  end subroutine get_abc_s


  subroutine get_abc_v(bc,dim,id,a,b,c)
    use class_dimensions
    use class_vector

    type(bc_poly), intent(in) :: bc
    type(dimensions), intent(in) :: dim
    integer, intent(out) :: id
    real(kind(1.d0)), intent(inout) :: a(:)
    real(kind(1.d0)), intent(inout) :: b(:)
    type(vector),     intent(inout) :: c(:)

    
  end subroutine get_abc_v



  function get_bc_motion_displacement(bc,x1,x2)result(res)
    use class_vector
    type(vector) :: res
    type(bc_poly), intent(in) :: bc
    real(kind(1.d0)), intent(in) :: x1, x2
    
    res = get_displacement(bc%mot,x1,x2)

  end function get_bc_motion_displacement


  function get_bc_motion_velocity(bc,x)result(res)
    use class_vector
    type(vector) :: res
    type(bc_poly), intent(in) :: bc
    real(kind(1.d0)), intent(in) :: x

    res = get_velocity(bc%mot,x)

  end function get_bc_motion_velocity


end module class_bc

module tools_mesh_basics
  
  implicit none
  
  interface
     function geom_tet_center(v1,v2,v3,v4)
       use class_vector
       type(vector) :: geom_tet_center
       type(vector), intent(in) :: v1, v2, v3, v4
     end function geom_tet_center
  end interface


end module tools_mesh_basics


subroutine smooth_mesh

  use class_bc
  use class_vector
  use tools_mesh_basics

  implicit none

  type(vector) :: new_pos  ! the new vertex position, after smoothing

end subroutine smooth_mesh
! { dg-final { cleanup-modules "class_vector class_dimensions tools_math" } }
! { dg-final { cleanup-modules "class_motion class_bc_math class_bc tools_mesh_basics" } }
