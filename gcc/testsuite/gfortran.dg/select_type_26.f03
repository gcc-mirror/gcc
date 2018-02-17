! { dg-do run }
! Tests fix for PR41600 and further SELECT TYPE functionality.
!
! Reported by Tobias Burnus  <burnus@gcc.gnu.org>
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
    class(t0), allocatable :: foo(:)
  end type t

  type t_scalar
    integer :: i
    class(t0), allocatable :: foo
  end type t_scalar

  type(t) :: m
  type(t_scalar) :: m1(4)
  integer :: n

! Test the fix for PR41600 itself - first with m%foo of declared type.
  allocate(m%foo(3), source = [(t0(n), n = 1,3)])
  select type(bar => m%foo)
    type is(t0)
      if (any (bar%j .ne. [1,2,3])) STOP 1
    type is(t1)
      STOP 2
  end select

  deallocate(m%foo)
  allocate(m%foo(3), source = [(t1(n, n*10), n = 4,6)])

! Then with m%foo of another dynamic type.
  select type(bar => m%foo)
    type is(t0)
      STOP 3
    type is(t1)
      if (any (bar%k .ne. [40,50,60])) STOP 4
  end select

! Try it with a selector array section.
  select type(bar => m%foo(2:3))
    type is(t0)
      STOP 5
    type is(t1)
      if (any (bar%k .ne. [50,60])) STOP 6
  end select

! Try it with a selector array element.
  select type(bar => m%foo(2))
    type is(t0)
      STOP 7
    type is(t1)
      if (bar%k .ne. 50) STOP 8
  end select

! Now try class is and a selector which is an array section of an associate name.
  select type(bar => m%foo)
    type is(t0)
      STOP 9
    class is (t1)
      if (any (bar%j .ne. [4,5,6])) STOP 10
      select type (foobar => bar(3:2:-1))
        type is (t1)
          if (any (foobar%k .ne. [60,50])) STOP 11
        end select
  end select

! Now try class is and a selector which is an array element of an associate name.
  select type(bar => m%foo)
    type is(t0)
      STOP 12
    class is (t1)
      if (any (bar%j .ne. [4,5,6])) STOP 13
      select type (foobar => bar(2))
        type is (t1)
          if (foobar%k .ne. 50) STOP 14
        end select
  end select

! Check class a component of an element of an array. Note that an array of such
! objects cannot be allowed since the elements could have different dynamic types.
! (F2003 C614)
  do n = 1, 2
    allocate(m1(n)%foo, source = t1(n*99, n*999))
  end do
  do n = 3, 4
    allocate(m1(n)%foo, source = t0(n*99))
  end do
  select type(bar => m1(3)%foo)
    type is(t0)
      if (bar%j .ne. 297) STOP 15
    type is(t1)
      STOP 16
  end select
  select type(bar => m1(1)%foo)
    type is(t0)
      STOP 17
    type is(t1)
      if (bar%k .ne. 999) STOP 18
  end select
end
