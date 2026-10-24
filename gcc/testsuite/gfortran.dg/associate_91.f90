! { dg-do run }
! PR fortran/126968
! Component references through nested CLASS components of an associate name
! whose type was guessed wrongly ('wrong_t').  The chain can carry explicit
! or implicit '_data' references after each CLASS component.
!
module m
  type :: wrong_t
     integer :: c1 = 1
  end type
  type :: leaf_t
     integer :: bar = 0
  end type
  type, extends (leaf_t) :: leaf2_t
     integer :: baz = 7
  end type
  type :: mid_t
     real :: junk = 0.0
     class(leaf_t), allocatable :: c2(:)
  end type
  type :: outer_t
     class(mid_t), allocatable :: c1(:)
  end type
contains
  subroutine fill (y)
    class(outer_t) :: y
    integer :: k
    allocate (mid_t :: y%c1(5))
    do k = 1, 5
      allocate (leaf2_t :: y%c1(k)%c2(3))
      y%c1(k)%c2(:)%bar = [10*(k-3)+1, 10*(k-3)+2, 10*(k-3)+3]
    end do
  end subroutine
end module

program p
  use m
  implicit none
  integer :: i, ia(3)

  associate (q => tfcn ())
    ia = q%c1(4)%c2(:)%bar
  end associate
  if (any (ia /= [11, 12, 13])) stop 1

  associate (q => cfcn ())
    ia = q%c1(4)%c2(:)%bar
  end associate
  if (any (ia /= [11, 12, 13])) stop 2

  associate (q => cfcn ())
    i = q%c1(4)%c2(2)%bar
  end associate
  if (i /= 12) stop 3

  associate (q => tfcn ())
    if (.not. same_type_as (q%c1(4)%c2, leaf2_t ())) stop 4
    select type (z => q%c1(4)%c2)
      type is (leaf2_t)
        if (any (z%baz /= 7)) stop 5
      class default
        stop 6
    end select
  end associate

contains

  function tfcn () result (y)
    type(outer_t) :: y
    call fill (y)
  end function

  function cfcn () result (y)
    class(outer_t), allocatable :: y
    allocate (y)
    call fill (y)
  end function
end program
