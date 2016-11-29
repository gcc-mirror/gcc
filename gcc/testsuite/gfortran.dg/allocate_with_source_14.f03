! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Tests the fix for PR61830.
!
! Contributed by Salvatore Filippone  <sfilippone@uniroma2.it>
!
module foo_base_mod
  integer, parameter :: foo_dpk_ = kind(1.d0)
  type foo_d_base_vect_type
    real(foo_dpk_), allocatable :: v(:)
  contains
    procedure :: free     => d_base_free
    procedure :: get_vect => d_base_get_vect
    procedure :: allocate => d_base_allocate
  end type foo_d_base_vect_type


  type foo_d_vect_type
    class(foo_d_base_vect_type), allocatable :: v
  contains
    procedure :: free     => d_vect_free
    procedure :: get_vect => d_vect_get_vect
  end type foo_d_vect_type

  type foo_desc_type
    integer :: nl=-1
  end type foo_desc_type

contains

  subroutine  foo_cdall(map,nl)
    type(foo_desc_type) :: map
    integer, optional  :: nl

    if (present(nl)) then
      map%nl = nl
    else
      map%nl = 1
    end if
  end subroutine foo_cdall


  subroutine  foo_cdasb(map,info)
    integer :: info
    type(foo_desc_type) :: map
    if (map%nl < 0) map%nl=1
  end subroutine foo_cdasb



  subroutine d_base_allocate(this,n)
    class(foo_d_base_vect_type), intent(out) :: this

    allocate(this%v(max(1,n)))

  end subroutine d_base_allocate

  subroutine d_base_free(this)
    class(foo_d_base_vect_type), intent(inout) :: this
    if (allocated(this%v))  then
      write(0,*) 'Scalar deallocation'
      deallocate(this%v)
    end if
  end subroutine d_base_free

  function d_base_get_vect(this) result(res)
    class(foo_d_base_vect_type), intent(inout) :: this
    real(foo_dpk_), allocatable :: res(:)

    if (allocated(this%v)) then
      res = this%v
    else
      allocate(res(1))
    end if
  end function d_base_get_vect

  subroutine d_vect_free(this)
    class(foo_d_vect_type) :: this
    if (allocated(this%v)) then
      call this%v%free()
      write(0,*) 'Deallocate class() component'
      deallocate(this%v)
    end if
  end subroutine d_vect_free

  function d_vect_get_vect(this) result(res)
    class(foo_d_vect_type) :: this
    real(foo_dpk_), allocatable :: res(:)

    if (allocated(this%v)) then
      res = this%v%get_vect()
    else
      allocate(res(1))
    end if
  end function d_vect_get_vect

  subroutine foo_geall(v,map,info)
    type(foo_d_vect_type), intent(out) :: v
    type(foo_Desc_type) :: map
    integer :: info

    allocate(foo_d_base_vect_type :: v%v,stat=info)
    if (info == 0) call v%v%allocate(map%nl)
  end subroutine foo_geall

end module foo_base_mod


module foo_scalar_field_mod
  use foo_base_mod
  implicit none

  type scalar_field
    type(foo_d_vect_type)        :: f
    type(foo_desc_type), pointer :: map => null()
  contains
    procedure :: free
  end type

  integer, parameter :: nx=4,ny=nx, nz=nx
  type(foo_desc_type), allocatable, save, target :: map
  integer ,save :: NumMy_xy_planes
  integer ,parameter :: NumGlobalElements = nx*ny*nz
  integer ,parameter :: NumGlobal_xy_planes = nz, &
       & Num_xy_points_per_plane = nx*ny

contains
  subroutine initialize_map(NumMyElements)
    integer :: NumMyElements, info
    info = 0
    if (allocated(map)) deallocate(map,stat=info)
    if (info == 0) allocate(map,stat=info)
    if (info == 0) call foo_cdall(map,nl=NumMyElements)
    if (info == 0) call foo_cdasb(map,info)
  end subroutine initialize_map

  function new_scalar_field() result(this)
    type(scalar_field)                          :: this
    real(foo_dpk_) ,allocatable   :: f_v(:)
    integer :: i,j,k,NumMyElements, iam, np, info,ip
    integer, allocatable :: idxs(:)

    NumMy_xy_planes = NumGlobal_xy_planes
    NumMyElements = NumMy_xy_planes*Num_xy_points_per_plane
    if (.not. allocated(map)) call initialize_map(NumMyElements)
    this%map => map
    call foo_geall(this%f,this%map,info)
  end function

  subroutine free(this)
    class(scalar_field), intent(inout) :: this
    integer ::info
    call this%f%free()
  end subroutine free

end module foo_scalar_field_mod

module foo_vector_field_mod
  use foo_base_mod
  use foo_scalar_field_mod
  implicit none
  type vector_field
    type(scalar_field) :: u(1)
  end type vector_field
contains
  function new_vector_field() result(this)
    type(vector_field) :: this
    integer :: i
    do i=1, size(this%u)
      associate(sf=>this%u(i))
        sf = new_scalar_field()
      end associate
    end do
  end function

  subroutine free_v_field(this)
    class(vector_field), intent(inout) :: this
    integer :: i
    associate(vf=>this%u)
      do i=1, size(vf)
        call vf(i)%free()
      end do
    end associate
  end subroutine free_v_field

end module foo_vector_field_mod

program main
  use foo_base_mod
  use foo_vector_field_mod
  use foo_scalar_field_mod
  implicit none
  type(vector_field) :: u
  type(foo_d_vect_type) :: v
  real(foo_dpk_), allocatable :: av(:)
  integer  :: iam, np, i,info

  u = new_vector_field()
  call foo_geall(v,map,info)
  call free_v_field(u)
  do i=1,10
    u = new_vector_field()
    call free_v_field(u)
    av = v%get_vect()
  end do
! This gets rid of the "memory leak"
  if (associated (u%u(1)%map)) deallocate (u%u(1)%map)
  call free_v_field(u)
  call v%free()
  deallocate(av)
end program
! { dg-final { scan-tree-dump-times "__builtin_malloc" 22 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_free" 29 "original" } }
