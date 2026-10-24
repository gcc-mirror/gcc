! { dg-do run }
! PR fortran/128144
! Type-bound procedure and procedure pointer component calls through
! components of an associate name whose selector is a contained function
! that has not been parsed yet.  'other_t' has a component 'c2' with a
! binding and a procedure pointer of the same names, so it can be guessed
! wrongly.
!
module m
  implicit none
  abstract interface
    integer function ifn (k)
      integer, intent(in) :: k
    end function
    real function rfn (x)
      real, intent(in) :: x
    end function
  end interface
  type :: oleaf_t
     procedure (rfn), pointer, nopass :: pp => null ()
  contains
    procedure :: get => oget
    procedure :: put => oput
  end type
  type :: other_t
     type(oleaf_t) :: c2
  end type
  type :: leaf_t
     integer :: bar = 0
     procedure (ifn), pointer, nopass :: pp => null ()
  contains
    procedure :: get
    procedure :: put
    procedure :: eget
  end type
  type, extends (leaf_t) :: leaf2_t
     integer :: baz(4) = 7
  contains
    procedure :: get => get2
  end type
  type :: mid_t
     class(leaf_t), allocatable :: c2
     class(leaf_t), allocatable :: arr(:)
  end type
contains
  integer function oget (this)
    class(oleaf_t), intent(in) :: this
    oget = -1
  end function
  subroutine oput (this, k)
    class(oleaf_t), intent(in) :: this
    integer, intent(out) :: k
    k = -1
  end subroutine
  integer function get (this)
    class(leaf_t), intent(in) :: this
    get = this%bar + 100
  end function
  integer function get2 (this)
    class(leaf2_t), intent(in) :: this
    get2 = this%bar + 1000
  end function
  subroutine put (this, k)
    class(leaf_t), intent(in) :: this
    integer, intent(out) :: k
    k = this%bar + 200
  end subroutine
  elemental integer function eget (this)
    class(leaf_t), intent(in) :: this
    eget = this%bar * 10
  end function
  integer function dbl (k)
    integer, intent(in) :: k
    dbl = 2 * k
  end function
end module

program p
  use m
  implicit none
  integer :: i, j, k, s, r(3)

  associate (q => cfcn ())
    i = q%c2%get ()
    call q%c2%put (j)
    k = q%c2%pp (21)
    s = storage_size (q%c2)
    r = q%arr%eget ()
  end associate
  if (i /= 1005) stop 1
  if (j /= 205) stop 2
  if (k /= 42) stop 3
  if (s /= storage_size (leaf2_t ())) stop 4
  if (any (r /= [10, 20, 30])) stop 5

  associate (q => lfcn ())
    i = q%get ()
    call q%put (j)
  end associate
  if (i /= 1005) stop 6
  if (j /= 205) stop 7

contains

  function cfcn () result (y)
    class(mid_t), allocatable :: y
    allocate (y)
    allocate (leaf2_t :: y%c2)
    y%c2%bar = 5
    y%c2%pp => dbl
    allocate (y%arr(3))
    y%arr%bar = [1, 2, 3]
  end function

  function lfcn () result (y)
    class(leaf_t), allocatable :: y
    allocate (leaf2_t :: y)
    y%bar = 5
  end function
end program
