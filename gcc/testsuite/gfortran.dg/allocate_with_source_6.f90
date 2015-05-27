! { dg-do  run }
! { dg-options "-fbounds-check" }
!
! Contributed by Juergen Reuter
! Check that pr65548 is fixed and that the ICE is gone, when bounds-check
! is requested.
!

module selectors
  type :: selector_t
     integer, dimension(:), allocatable :: map
     real, dimension(:), allocatable :: weight
   contains
     procedure :: init => selector_init
   end type selector_t

contains

  subroutine selector_init (selector, weight)
    class(selector_t), intent(out) :: selector
    real, dimension(:), intent(in) :: weight
    real :: s
    integer :: n, i
    logical, dimension(:), allocatable :: mask
    s = sum (weight)
    allocate (mask (size (weight)), source = weight /= 0)
    n = count (mask)
    if (n > 0) then
       allocate (selector%map (n), &
            source = pack ([(i, i = 1, size (weight))], mask))
       allocate (selector%weight (n), &
            source = pack (weight / s, mask))
    else
       allocate (selector%map (1), source = 1)
       allocate (selector%weight (1), source = 0.)
    end if
  end subroutine selector_init

end module selectors

module phs_base
  type :: flavor_t
  contains
     procedure :: get_mass => flavor_get_mass
  end type flavor_t

  type :: phs_config_t
     integer :: n_in = 0
     type(flavor_t), dimension(:,:), allocatable :: flv
  end type phs_config_t

  type :: phs_t
     class(phs_config_t), pointer :: config => null ()
     real, dimension(:), allocatable :: m_in
  end type phs_t

contains

  elemental function flavor_get_mass (flv) result (mass)
    real :: mass
    class(flavor_t), intent(in) :: flv
    mass = 42.0
  end function flavor_get_mass

  subroutine phs_base_init (phs, phs_config)
    class(phs_t), intent(out) :: phs
    class(phs_config_t), intent(in), target :: phs_config
    phs%config => phs_config
    allocate (phs%m_in  (phs%config%n_in), &
         source = phs_config%flv(:phs_config%n_in, 1)%get_mass ())
  end subroutine phs_base_init

end module phs_base

module foo
  type :: t
     integer :: n
     real, dimension(:,:), allocatable :: val
   contains
     procedure :: make => t_make
     generic :: get_int => get_int_array, get_int_element
     procedure :: get_int_array => t_get_int_array
     procedure :: get_int_element => t_get_int_element
  end type t

contains

  subroutine t_make (this)
    class(t), intent(inout) :: this
    real, dimension(:), allocatable :: int
    allocate (int (0:this%n-1), source=this%get_int())
  end subroutine t_make

  pure function t_get_int_array (this) result (array)
    class(t), intent(in) :: this
    real, dimension(this%n) :: array
    array = this%val (0:this%n-1, 4)
  end function t_get_int_array

  pure function t_get_int_element (this, set) result (element)
    class(t), intent(in) :: this
    integer, intent(in) :: set
    real :: element
    element = this%val (set, 4)
  end function t_get_int_element
end module foo
module foo2
  type :: t2
     integer :: n
     character(32), dimension(:), allocatable :: md5
   contains
     procedure :: init => t2_init
  end type t2

contains

  subroutine t2_init (this)
    class(t2), intent(inout) :: this
    character(32), dimension(:), allocatable :: md5
    allocate (md5 (this%n), source=this%md5)
    if (md5(1) /= "tst                             ") call abort()
    if (md5(2) /= "                                ") call abort()
    if (md5(3) /= "fooblabar                       ") call abort()
  end subroutine t2_init
end module foo2

program test
  use selectors
  use phs_base
  use foo
  use foo2

  type(selector_t) :: sel
  type(phs_t) :: phs
  type(phs_config_t) :: phs_config
  type(t) :: o
  type(t2) :: o2

  call sel%init([2., 0., 3., 0., 4.])

  if (any(sel%map /= [1, 3, 5])) call abort()
  if (any(abs(sel%weight - [2., 3., 4.] / 9.) > 1E-6)) call abort()

  phs_config%n_in = 2
  allocate (phs_config%flv (phs_config%n_in, 1))
  call phs_base_init (phs, phs_config)

  if (any(abs(phs%m_in - [42.0, 42.0]) > 1E-6)) call abort()

  o%n = 2
  allocate (o%val(0:1,4))
  call o%make()

  o2%n = 3
  allocate(o2%md5(o2%n))
  o2%md5(1) = "tst"
  o2%md5(2) = ""
  o2%md5(3) = "fooblabar"
  call o2%init()
end program test

