! { dg-do run }
!
!     Solve a diffusion problem using an object-oriented approach
!
!     Author: Arjen Markus (comp.lang.fortran)
!     This version: pault@gcc.gnu.org
!
!     Note:
!     (i) This could be turned into a more sophisticated program
!     using the techniques described in the chapter on
!     mathematical abstractions.
!     (That would allow the selection of the time integration
!     method in a transparent way)
!
!     (ii) The target procedures for process_p and source_p are
!     different to the typebound procedures for dynamic types
!     because the passed argument is not type(base_pde_object).
!
!     (iii) Two solutions are calculated, one with the procedure
!     pointers and the other with typebound procedures. The sums
!     of the solutions are compared.

!     (iv) The source is a delta function in the middle of the
!     mesh, whilst the process is quartic in the local value,
!     when it is positive.
!
! base_pde_objects --
!     Module to define the basic objects
!
module base_pde_objects
  implicit none
  type, abstract :: base_pde_object
! No data
    procedure(process_p), pointer, pass :: process_p
    procedure(source_p), pointer, pass  :: source_p
  contains
    procedure(process), deferred :: process
    procedure(source), deferred :: source
    procedure :: initialise
    procedure :: nabla2
    procedure :: print
    procedure(real_times_obj), pass(obj), deferred :: real_times_obj
    procedure(obj_plus_obj),              deferred :: obj_plus_obj
    procedure(obj_assign_obj),            deferred :: obj_assign_obj
    generic :: operator(*)    => real_times_obj
    generic :: operator(+)    => obj_plus_obj
    generic :: assignment(=)  => obj_assign_obj
  end type
  abstract interface
    function process_p (obj)
      import base_pde_object
      class(base_pde_object), intent(in)  :: obj
      class(base_pde_object), allocatable :: process_p
    end function process_p
  end interface
  abstract interface
    function source_p (obj, time)
      import base_pde_object
      class(base_pde_object), intent(in)  :: obj
      real, intent(in)                    :: time
      class(base_pde_object), allocatable :: source_p
    end function source_p
  end interface
  abstract interface
    function process (obj)
      import base_pde_object
      class(base_pde_object), intent(in)  :: obj
      class(base_pde_object), allocatable :: process
    end function process
  end interface
  abstract interface
    function source (obj, time)
      import base_pde_object
      class(base_pde_object), intent(in)  :: obj
      real, intent(in)                    :: time
      class(base_pde_object), allocatable :: source
    end function source
  end interface
  abstract interface
    function real_times_obj (factor, obj) result(newobj)
      import base_pde_object
      real, intent(in)                    :: factor
      class(base_pde_object), intent(in)  :: obj
      class(base_pde_object), allocatable :: newobj
    end function real_times_obj
  end interface
  abstract interface
    function obj_plus_obj (obj1, obj2) result(newobj)
      import base_pde_object
      class(base_pde_object), intent(in)  :: obj1
      class(base_pde_object), intent(in)  :: obj2
      class(base_pde_object), allocatable :: newobj
    end function obj_plus_obj
  end interface
  abstract interface
    subroutine obj_assign_obj (obj1, obj2)
      import base_pde_object
      class(base_pde_object), intent(inout)  :: obj1
      class(base_pde_object), intent(in)     :: obj2
    end subroutine obj_assign_obj
  end interface
contains
! print --
!     Print the concentration field
  subroutine print (obj)
    class(base_pde_object) :: obj
    ! Dummy
  end subroutine print
! initialise --
!     Initialise the concentration field using a specific function
  subroutine initialise (obj, funcxy)
    class(base_pde_object) :: obj
    interface
      real function funcxy (coords)
        real, dimension(:), intent(in) :: coords
      end function funcxy
    end interface
    ! Dummy
  end subroutine initialise
! nabla2 --
!     Determine the divergence
  function nabla2 (obj)
    class(base_pde_object), intent(in)  :: obj
    class(base_pde_object), allocatable :: nabla2
    ! Dummy
  end function nabla2
end module base_pde_objects
! cartesian_2d_objects --
!     PDE object on a 2D cartesian grid
!
module cartesian_2d_objects
  use base_pde_objects
  implicit none
  type, extends(base_pde_object) :: cartesian_2d_object
    real, dimension(:,:), allocatable :: c
    real                              :: dx
    real                              :: dy
  contains
    procedure            :: process       => process_cart2d
    procedure            :: source         => source_cart2d
    procedure            :: initialise     => initialise_cart2d
    procedure            :: nabla2         => nabla2_cart2d
    procedure            :: print          => print_cart2d
    procedure, pass(obj) :: real_times_obj => real_times_cart2d
    procedure            :: obj_plus_obj   => obj_plus_cart2d
    procedure            :: obj_assign_obj => obj_assign_cart2d
  end type cartesian_2d_object
  interface grid_definition
    module procedure grid_definition_cart2d
  end interface
contains
  function process_cart2d (obj)
    class(cartesian_2d_object), intent(in)  :: obj
    class(base_pde_object), allocatable :: process_cart2d
    allocate (process_cart2d,source = obj)
    select type (process_cart2d)
      type is (cartesian_2d_object)
        process_cart2d%c = -sign (obj%c, 1.0)*obj%c** 4
      class default
        call abort
    end select
  end function process_cart2d
  function process_cart2d_p (obj)
    class(base_pde_object), intent(in)  :: obj
    class(base_pde_object), allocatable :: process_cart2d_p
    allocate (process_cart2d_p,source = obj)
    select type (process_cart2d_p)
      type is (cartesian_2d_object)
        select type (obj)
          type is (cartesian_2d_object)
            process_cart2d_p%c = -sign (obj%c, 1.0)*obj%c** 4
        end select
      class default
        call abort
    end select
  end function process_cart2d_p
  function source_cart2d (obj, time)
    class(cartesian_2d_object), intent(in)  :: obj
    real, intent(in)                    :: time
    class(base_pde_object), allocatable :: source_cart2d
    integer :: m, n
    m = size (obj%c, 1)
    n = size (obj%c, 2)
    allocate (source_cart2d, source = obj)
    select type (source_cart2d)
      type is (cartesian_2d_object)
        if (allocated (source_cart2d%c)) deallocate (source_cart2d%c)
        allocate (source_cart2d%c(m, n))
        source_cart2d%c = 0.0
        if (time .lt. 5.0) source_cart2d%c(m/2, n/2) = 0.1
      class default
        call abort
    end select
  end function source_cart2d

  function source_cart2d_p (obj, time)
    class(base_pde_object), intent(in)  :: obj
    real, intent(in)                    :: time
    class(base_pde_object), allocatable :: source_cart2d_p
    integer :: m, n
    select type (obj)
      type is (cartesian_2d_object)
        m = size (obj%c, 1)
        n = size (obj%c, 2)
      class default
       call abort
    end select
    allocate (source_cart2d_p,source = obj)
    select type (source_cart2d_p)
      type is (cartesian_2d_object)
        if (allocated (source_cart2d_p%c)) deallocate (source_cart2d_p%c)
        allocate (source_cart2d_p%c(m,n))
        source_cart2d_p%c = 0.0
        if (time .lt. 5.0) source_cart2d_p%c(m/2, n/2) = 0.1
      class default
        call abort
    end select
  end function source_cart2d_p

! grid_definition --
!     Initialises the grid
!
  subroutine grid_definition_cart2d (obj, sizes, dims)
    class(base_pde_object), allocatable :: obj
    real, dimension(:)                  :: sizes
    integer, dimension(:)               :: dims
    allocate( cartesian_2d_object :: obj )
    select type (obj)
      type is (cartesian_2d_object)
        allocate (obj%c(dims(1), dims(2)))
        obj%c  = 0.0
        obj%dx = sizes(1)/dims(1)
        obj%dy = sizes(2)/dims(2)
      class default
        call abort
    end select
  end subroutine grid_definition_cart2d
! print_cart2d --
!     Print the concentration field to the screen
!
  subroutine print_cart2d (obj)
    class(cartesian_2d_object) :: obj
    character(len=20)          :: format
    write( format, '(a,i0,a)' ) '(', size(obj%c,1), 'f6.3)'
    write( *, format ) obj%c
  end subroutine print_cart2d
! initialise_cart2d --
!     Initialise the concentration field using a specific function
!
  subroutine initialise_cart2d (obj, funcxy)
    class(cartesian_2d_object) :: obj
    interface
      real function funcxy (coords)
        real, dimension(:), intent(in) :: coords
      end function funcxy
    end interface
    integer                    :: i, j
    real, dimension(2)         :: x
    obj%c = 0.0
    do j = 2,size (obj%c, 2)-1
      x(2) = obj%dy * (j-1)
      do i = 2,size (obj%c, 1)-1
        x(1) = obj%dx * (i-1)
        obj%c(i,j) = funcxy (x)
      enddo
    enddo
  end subroutine initialise_cart2d
! nabla2_cart2d
!     Determine the divergence
  function nabla2_cart2d (obj)
    class(cartesian_2d_object), intent(in)  :: obj
    class(base_pde_object), allocatable     :: nabla2_cart2d
    integer                                 :: m, n
    real                                    :: dx, dy
    m = size (obj%c, 1)
    n = size (obj%c, 2)
    dx = obj%dx
    dy = obj%dy
    allocate (cartesian_2d_object :: nabla2_cart2d)
    select type (nabla2_cart2d)
      type is (cartesian_2d_object)
        allocate (nabla2_cart2d%c(m,n))
        nabla2_cart2d%c = 0.0
        nabla2_cart2d%c(2:m-1,2:n-1) = &
          -(2.0 * obj%c(2:m-1,2:n-1) - obj%c(1:m-2,2:n-1) - obj%c(3:m,2:n-1)) / dx**2 &
          -(2.0 * obj%c(2:m-1,2:n-1) - obj%c(2:m-1,1:n-2) - obj%c(2:m-1,3:n)) / dy**2
      class default
        call abort
    end select
  end function nabla2_cart2d
  function real_times_cart2d (factor, obj) result(newobj)
    real, intent(in)                        :: factor
    class(cartesian_2d_object), intent(in)  :: obj
    class(base_pde_object), allocatable     :: newobj
    integer                                 :: m, n
    m = size (obj%c, 1)
    n = size (obj%c, 2)
    allocate (cartesian_2d_object :: newobj)
    select type (newobj)
      type is (cartesian_2d_object)
        allocate (newobj%c(m,n))
        newobj%c = factor * obj%c
      class default
        call abort
    end select
  end function real_times_cart2d
  function obj_plus_cart2d (obj1, obj2) result( newobj )
    class(cartesian_2d_object), intent(in)  :: obj1
    class(base_pde_object), intent(in)      :: obj2
    class(base_pde_object), allocatable     :: newobj
    integer                                 :: m, n
    m = size (obj1%c, 1)
    n = size (obj1%c, 2)
    allocate (cartesian_2d_object :: newobj)
    select type (newobj)
      type is (cartesian_2d_object)
        allocate (newobj%c(m,n))
          select type (obj2)
            type is (cartesian_2d_object)
              newobj%c = obj1%c + obj2%c
            class default
              call abort
          end select
      class default
        call abort
    end select
  end function obj_plus_cart2d
  subroutine obj_assign_cart2d (obj1, obj2)
    class(cartesian_2d_object), intent(inout) :: obj1
    class(base_pde_object), intent(in)        :: obj2
    select type (obj2)
      type is (cartesian_2d_object)
        obj1%c = obj2%c
      class default
        call abort
    end select
  end subroutine obj_assign_cart2d
end module cartesian_2d_objects
! define_pde_objects --
!     Module to bring all the PDE object types together
!
module define_pde_objects
  use base_pde_objects
  use cartesian_2d_objects
  implicit none
  interface grid_definition
    module procedure grid_definition_general
  end interface
contains
  subroutine grid_definition_general (obj, type, sizes, dims)
    class(base_pde_object), allocatable :: obj
    character(len=*)                    :: type
    real, dimension(:)                  :: sizes
    integer, dimension(:)               :: dims
    select case (type)
      case ("cartesian 2d")
        call grid_definition (obj, sizes, dims)
      case default
        write(*,*) 'Unknown grid type: ', trim (type)
        stop
    end select
  end subroutine grid_definition_general
end module define_pde_objects
! pde_specific --
!     Module holding the routines specific to the PDE that
!     we are solving
!
module pde_specific
  implicit none
contains
  real function patch (coords)
    real, dimension(:), intent(in) :: coords
    if (sum ((coords-[50.0,50.0])**2) < 40.0) then
      patch = 1.0
    else
      patch = 0.0
    endif
  end function patch
end module pde_specific
! test_pde_solver --
!     Small test program to demonstrate the usage
!
program test_pde_solver
  use define_pde_objects
  use pde_specific
  implicit none
  class(base_pde_object), allocatable :: solution, deriv
  integer                             :: i
  real                                :: time, dtime, diff, chksum(2)

  call simulation1     ! Use proc pointers for source and process define_pde_objects
  select type (solution)
    type is (cartesian_2d_object)
      deallocate (solution%c)
  end select
  select type (deriv)
    type is (cartesian_2d_object)
      deallocate (deriv%c)
  end select
  deallocate (solution, deriv)

  call simulation2     ! Use typebound procedures for source and process
  if (chksum(1) .ne. chksum(2)) call abort
  if ((chksum(1) - 0.881868720)**2 > 1e-4) call abort
contains
  subroutine simulation1
!
! Create the grid
!
    call grid_definition (solution, "cartesian 2d", [100.0, 100.0], [16, 16])
    call grid_definition (deriv,    "cartesian 2d", [100.0, 100.0], [16, 16])
!
! Initialise the concentration field
!
    call solution%initialise (patch)
!
! Set the procedure pointers
!
    solution%source_p => source_cart2d_p
    solution%process_p => process_cart2d_p
!
! Perform the integration - explicit method
!
    time  = 0.0
    dtime = 0.1
    diff =  5.0e-3

! Give the diffusion coefficient correct dimensions.
    select type (solution)
      type is (cartesian_2d_object)
        diff  = diff * solution%dx * solution%dy / dtime
    end select

!     write(*,*) 'Time: ', time, diff
!     call solution%print
    do i = 1,100
      deriv    =  solution%nabla2 ()
      solution = solution + diff * dtime * deriv + solution%source_p (time) + solution%process_p ()
!         if ( mod(i, 25) == 0 ) then
!             write(*,*)'Time: ', time
!             call solution%print
!         endif
    time = time + dtime
    enddo
!    write(*,*) 'End result 1: '
!    call solution%print
    select type (solution)
      type is (cartesian_2d_object)
        chksum(1) = sum (solution%c)
    end select
  end subroutine
  subroutine simulation2
!
! Create the grid
!
    call grid_definition (solution, "cartesian 2d", [100.0, 100.0], [16, 16])
    call grid_definition (deriv,    "cartesian 2d", [100.0, 100.0], [16, 16])
!
! Initialise the concentration field
!
    call solution%initialise (patch)
!
! Set the procedure pointers
!
    solution%source_p => source_cart2d_p
    solution%process_p => process_cart2d_p
!
! Perform the integration - explicit method
!
    time  = 0.0
    dtime = 0.1
    diff =  5.0e-3

! Give the diffusion coefficient correct dimensions.
    select type (solution)
      type is (cartesian_2d_object)
        diff  = diff * solution%dx * solution%dy / dtime
    end select

!     write(*,*) 'Time: ', time, diff
!     call solution%print
    do i = 1,100
      deriv    =  solution%nabla2 ()
      solution = solution + diff * dtime * deriv + solution%source (time) + solution%process ()
!         if ( mod(i, 25) == 0 ) then
!             write(*,*)'Time: ', time
!             call solution%print
!         endif
      time = time + dtime
    enddo
!    write(*,*) 'End result 2: '
!    call solution%print
    select type (solution)
      type is (cartesian_2d_object)
        chksum(2) = sum (solution%c)
    end select
  end subroutine
end program test_pde_solver
! { dg-final { cleanup-modules "pde_specific define_pde_objects cartesian_2d_objects base_pde_objects" } }
