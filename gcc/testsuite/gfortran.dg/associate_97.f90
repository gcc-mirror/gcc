! { dg-do run }
! PR fortran/126968
! The parse-time type guess for an associate name must pick a candidate
! type in which the whole component chain exists ('wrong_t' and 'zz_t' are
! tried first).
!
module m
  type :: leaf_t
     integer :: bar = 0
     complex :: z = (1.0, 2.0)
     character(5) :: s = 'hello'
   contains
     procedure :: get
  end type
  type :: mid_t
     class(leaf_t), allocatable :: c2(:)
  end type
  type :: outer_t
     type(mid_t) :: c1(5)
  end type
  type :: wrong_t
     integer :: c1 = 1
  end type
  type :: zz_t
     character(8) :: c1 = 'zzzzzzzz'
  end type
contains
  integer function get (this)
    class(leaf_t), intent(in) :: this
    get = this%bar + 100
  end function
end module

program p
  use m
  implicit none
  integer :: i, k
  real :: r
  character(3) :: c

  associate (q => tfcn ())
    i = q%c1(index ("a(b)c", "(") + 2)%c2(2)%bar
    if (i /= 42) stop 1
    i = q%c1(3)% &
        c2(1)%bar
    if (i /= 31) stop 2
    r = q%c1(4)%c2(1)%z%im
    if (r /= 2.0) stop 3
    k = q%c1(4)%c2(3)%s%len
    if (k /= 5) stop 4
    c = q%c1(4)%c2(3)%s(2:4)
    if (c /= 'ell') stop 5
    i = q%c1(2)%c2(1)%get ()
    if (i /= 121) stop 6
  end associate

contains

  function tfcn () result (y)
    type(outer_t) :: y
    integer :: k
    do k = 1, 5
      allocate (y%c1(k)%c2(3))
      y%c1(k)%c2(:)%bar = [10*k+1, 10*k+2, 10*k+3]
    end do
  end function
end program
