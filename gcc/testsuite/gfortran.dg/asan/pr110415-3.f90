! { dg-do run }
!
! Contributed by Brad Richardson <everythingfunctional@protonmail.com>
!
implicit none
  type, abstract :: p
    integer :: a = 4
  end type p

  type, extends(p) :: c
    integer :: b = 7
    character(len=:), allocatable :: str, str2(:)
  end type c

  type, extends(p) :: d
    integer :: ef = 7
  end type d

  class(p), allocatable :: a(:)

  a = func()

  a = func2()

  a = func()

  deallocate(a)

contains
  function func2() result(a)
    class(p), allocatable :: a(:)
    a = [d(),d()]
  end function func2

  function func() result(a)
    class(p), allocatable :: a(:)

    a = [c(),c(),c()]
    select type(a)
    type is (c)
      a(1)%str = 'abcd'
      a(2)%str = 'abc'
      a(3)%str = 'abcd4'
      a(1)%str2 = ['abcd','efgh']
      a(2)%str2 = ['bcd','fgh']
      a(3)%str2 = ['abcd6','efgh7']
    end select
  end function func
end program
