! { dg-do run }
! PR fortran/126968
! Array references after components of an associate name whose type was
! guessed wrongly ('g*_t') must follow the array spec of the real component.
!
module m
  type :: leaf_t
     integer :: bar = 0
  end type
  type :: w1_t
     type(leaf_t) :: b(2)
  end type
  type :: g_t
     type(w1_t) :: a(4)
     integer :: v(3) = 9
     character(4) :: s(3) = 'gggg'
     integer :: m(3) = 9
  end type
  type :: c1_t
     type(leaf_t) :: b(3)
  end type
  type :: r_t
     type(c1_t) :: a(4)
     integer :: v = 5
     character(4) :: s = 'abcd'
     integer :: m(2,2) = reshape ([1, 2, 3, 4], [2, 2])
  end type
  type :: g2_t
     integer :: w = 9
     character(4) :: t = 'gggg'
     class(leaf_t), allocatable :: c
  end type
  type :: r2_t
     integer :: w(3) = [1, 2, 3]
     character(4) :: t(3) = ['aaaa', 'bbbb', 'cccc']
     class(leaf_t), allocatable :: c(:)
  end type
end module

program p
  use m
  implicit none
  integer :: i, ia(3), im(2,2), j(2)
  character(2) :: c
  character(4) :: ca(2)

  associate (q => fcn ())
    ia = q%a(2)%b(:)%bar
    i = q%v
    c = q%s(2:3)
    im = q%m
    j = q%m(:,2)
  end associate
  if (any (ia /= [1, 2, 3])) stop 1
  if (i /= 5) stop 2
  if (c /= 'bc') stop 3
  if (any (im /= reshape ([1, 2, 3, 4], [2, 2]))) stop 4
  if (any (j /= [3, 4])) stop 5

  associate (q => fcn2 ())
    ia = q%w
    ca = q%t(2:3)
    if (q%t%len /= 4) stop 6
    if (size (q%c) /= 3) stop 7
    if (any (q%c%bar /= 7)) stop 8
  end associate
  if (any (ia /= [1, 2, 3])) stop 9
  if (any (ca /= ['bbbb', 'cccc'])) stop 10

contains

  function fcn () result (y)
    type(r_t) :: y
    integer :: k
    do k = 1, 4
      y%a(k)%b(:)%bar = [1, 2, 3]
    end do
  end function

  function fcn2 () result (y)
    type(r2_t) :: y
    allocate (y%c(3), source = leaf_t (7))
  end function
end program
