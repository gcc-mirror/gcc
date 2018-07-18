! { dg-do compile }
!
! Fix for PR53191
!
  implicit none
  type t0
    integer :: j = 42
  end type t0
  type, extends(t0) :: t1
    integer :: k = 99
  end type t1
  type t
    integer :: i
    class(t0), allocatable :: foo
  end type t
  type(t) :: m(4)
  integer :: n

  do n = 1, 2
    allocate(m(n)%foo, source = t0(n*99))
  end do
  do n = 3, 4
    allocate(m(n)%foo, source = t1(n*99, n*999))
  end do

! An array of objects with ultimate class components cannot be a selector
! since each element could have a different dynamic type. (F2003 C614)

  select type(bar => m%foo) ! { dg-error "part reference with nonzero rank" }
    type is(t0)
      if (any (bar%j .ne. [99, 198, 297, 396])) STOP 1
    type is(t1)
      STOP 2
  end select

end
