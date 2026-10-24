! { dg-do run }
! PR fortran/128144
! References to CLASS array components of an element of an associate name
! whose selector is an array valued contained function that has not been
! parsed yet.
!
module m
  implicit none
  type :: leaf_t
     integer :: bar = 0
  contains
    procedure :: get
  end type
  type, extends (leaf_t) :: leaf2_t
  contains
    procedure :: get => get2
  end type
  type :: mid_t
     class(leaf_t), allocatable :: c2(:)
  end type
  type :: top_t
     class(leaf_t), pointer :: c2(:,:) => null ()
     class(mid_t), allocatable :: cm
  end type
contains
  integer function get (this)
    class(leaf_t), intent(in) :: this
    get = this%bar + 100
  end function
  integer function get2 (this)
    class(leaf2_t), intent(in) :: this
    get2 = this%bar + 1000
  end function
end module

program p
  use m
  implicit none
  integer :: i, s(2)

  associate (q => cfcn ())
    if (size (q(1)%cm%c2) /= 3) stop 1
    if (any (q(1)%cm%c2%bar /= [1, 2, 3])) stop 2
    i = q(1)%cm%c2(2)%get ()
    if (i /= 1002) stop 3
    s = shape (q(2)%c2)
    if (any (s /= [2, 3])) stop 4
    if (q(2)%c2(2,3)%bar /= 6) stop 5
    i = q(2)%c2(1,2)%get ()
    if (i /= 1003) stop 6
  end associate

contains

  function cfcn () result (y)
    type(top_t) :: y(2)
    allocate (y(1)%cm)
    allocate (leaf2_t :: y(1)%cm%c2(3))
    y(1)%cm%c2%bar = [1, 2, 3]
    allocate (leaf2_t :: y(2)%c2(2,3))
    y(2)%c2%bar = reshape ([1, 2, 3, 4, 5, 6], [2, 3])
  end function
end program
