! { dg-do compile }
!
! Tests the fix for PR61819.
!
! Contributed by Salvatore Filippone  <sfilippone@uniroma2.it>
!
module foo_base_mod
  integer, parameter :: foo_ipk_ = kind(1)
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
    integer(foo_ipk_) :: nl=-1
  end type foo_desc_type


contains

  subroutine foo_init(ictxt)
    integer :: ictxt
  end subroutine foo_init


  subroutine foo_exit(ictxt)
    integer :: ictxt
  end subroutine foo_exit

  subroutine foo_info(ictxt,iam,np)
    integer(foo_ipk_) :: ictxt,iam,np
    iam = 0
    np = 1
  end subroutine foo_info

  subroutine  foo_cdall(ictxt,map,info,nl)
    integer(foo_ipk_) :: ictxt, info
    type(foo_desc_type) :: map
    integer(foo_ipk_), optional  :: nl

    if (present(nl)) then
      map%nl = nl
    else
      map%nl = 1
    end if
  end subroutine foo_cdall

  subroutine  foo_cdasb(map,info)
    integer(foo_ipk_) :: info
    type(foo_desc_type) :: map
    if (map%nl < 0) map%nl=1
  end subroutine foo_cdasb


  subroutine d_base_allocate(this,n)
    class(foo_d_base_vect_type), intent(out) :: this

    allocate(this%v(max(1,n)))

  end subroutine d_base_allocate

  subroutine d_base_free(this)
    class(foo_d_base_vect_type), intent(inout) :: this
    if (allocated(this%v)) &
         & deallocate(this%v)
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
    integer(foo_ipk_) :: info

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

  integer(foo_ipk_), parameter :: nx=4,ny=nx, nz=nx
  type(foo_desc_type), allocatable, save, target :: map
  integer(foo_ipk_) ,save :: NumMy_xy_planes
  integer(foo_ipk_) ,parameter :: NumGlobalElements = nx*ny*nz
  integer(foo_ipk_) ,parameter :: NumGlobal_xy_planes = nz, Num_xy_points_per_plane = nx*ny

contains
  subroutine initialize_map(ictxt,NumMyElements,info)
    integer(foo_ipk_) :: ictxt, NumMyElements, info
    info = 0
    if (allocated(map)) deallocate(map,stat=info)
    if (info == 0) allocate(map,stat=info)
    if (info == 0) call foo_cdall(ictxt,map,info,nl=NumMyElements)
    if (info == 0) call foo_cdasb(map,info)
  end subroutine initialize_map

  function new_scalar_field(comm) result(this)
    type(scalar_field)                          :: this
    integer(foo_ipk_)              ,intent(in) :: comm
    real(foo_dpk_) ,allocatable   :: f_v(:)
    integer(foo_ipk_) :: i,j,k,NumMyElements, iam, np, info,ip
    integer(foo_ipk_), allocatable :: idxs(:)
    call foo_info(comm,iam,np)
    NumMy_xy_planes = NumGlobal_xy_planes/np
    NumMyElements = NumMy_xy_planes*Num_xy_points_per_plane
    if (.not. allocated(map)) call initialize_map(comm,NumMyElements,info)
    this%map => map
    call foo_geall(this%f,this%map,info)
  end function

  subroutine free(this)
    class(scalar_field), intent(inout) :: this
    integer(foo_ipk_) ::info
    write(0,*) 'Freeing scalar_this%f'
    call this%f%free()
  end subroutine free

end module foo_scalar_field_mod

module foo_vector_field_mod
  use foo_base_mod
  use foo_scalar_field_mod, only : scalar_field,new_scalar_field
  implicit none
  type vector_field
    type(scalar_field) :: u(1)
  contains
    procedure :: free
  end type
contains
  function new_vector_field(comm_in) result(this)
    type(vector_field) :: this
    integer(foo_ipk_), intent(in) :: comm_in
    this%u = [new_scalar_field(comm_in)] ! Removing this line eliminates the memory leak
  end function

  subroutine free(this)
    class(vector_field), intent(inout) :: this
    integer :: i
    associate(vf=>this%u)
      do i=1, size(vf)
        write(0,*) 'Freeing vector_this%u(',i,')'
        call vf(i)%free()
      end do
    end associate
  end subroutine free

end module foo_vector_field_mod

program main
  use foo_base_mod
  use foo_vector_field_mod,only: vector_field,new_vector_field
  use foo_scalar_field_mod,only: map
  implicit none
  type(vector_field) :: u
  type(foo_d_vect_type) :: v
  real(foo_dpk_), allocatable :: av(:)
  integer(foo_ipk_) :: ictxt, iam, np, i,info
  call foo_init(ictxt)
  call foo_info(ictxt,iam,np)
  u = new_vector_field(ictxt)
  call u%free()
  do i=1,10
    u = new_vector_field(ictxt)
    call u%free()
  end do
  call u%free()
  call foo_exit(ictxt)
end program
