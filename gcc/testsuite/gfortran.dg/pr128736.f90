! { dg-do run }
!
! PR 128736

module m
  type t
    integer :: i(100)
  end type
  type, extends(t) :: t2
    integer :: j(50) = -1
  end type
contains
  function f (n) result (r)
    integer :: n
    class(t), allocatable :: r(:)
    integer :: k
    allocate (t2 :: r(n))
    do k = 1, n
      r(k)%i = k
    end do
  end function
end module

program p
  use m
  call s
contains
  subroutine s
    class(t), allocatable :: z(:)
    type(t), allocatable :: w(:)
    integer :: k
    allocate (z(1000))
    do k = 1, 1000
      z(k)%i = k
    end do
    w = z
    if (size (w) /= 1000 .or. any (w(1000)%i /= 1000)) stop 1
    deallocate (z)
    allocate (t2 :: z(10))
    do k = 1, 10
      z(k)%i = 10*k
    end do
    w = z(::3)
    if (size (w) /= 4 .or. any (w%i(1) /= [10, 40, 70, 100])) stop 2
    w = f (7)
    if (size (w) /= 7 .or. any (w(7)%i /= 7)) stop 3
  end subroutine
end program
