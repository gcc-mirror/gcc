! { dg-do run }
!
! Contributed by Brad Richardson <everythingfunctional@protonmail.com>
!
  type, abstract :: p
  end type p

  type, extends(p) :: c
  end type c

  class(p), allocatable :: a

  a = func()
contains
  function func() result(a)
    class(p), allocatable :: a

    a = c()
  end function func
end program
