! { dg-do compile }
!
! PR 80766: [7/8 Regression] [OOP] ICE with type-bound procedure returning an array
!
! Contributed by Vladimir Fuka <vladimir.fuka@gmail.com>

module m1

  type :: base
  contains
     procedure :: fun
  end type

  type, extends(base) :: child
  end type

contains

  function fun(o) result(res)
    real :: res(3)
    class(base) :: o
    res = 0
  end function
end module


module m2
contains

  subroutine sub(o)
    use m1
    class(child) :: o
    real :: res(3)

    res = o%fun()
  end subroutine
end module
