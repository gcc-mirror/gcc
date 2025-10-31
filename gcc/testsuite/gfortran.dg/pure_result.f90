! { dg-do compile }
! PR fortran/78640 - constraints on pure function results
!
! F2018:C1585, F2023:C1594:
! "The function result of a pure function shall not be both polymorphic and
!  allocatable, or have a polymorphic allocatable ultimate component."

program pr78640
  implicit none

  type foo_t
  end type

  type bar_t
     integer,  allocatable :: dummy
     class(*), allocatable :: c
  end type bar_t

contains

  pure function f() result(foo) ! { dg-error "is polymorphic allocatable" }
    class(foo_t), allocatable :: foo
    foo = foo_t()
  end function

  pure function f2() ! { dg-error "is polymorphic allocatable" }
    class(foo_t), allocatable :: f2
    f2 = foo_t()
  end function

  pure function g() result(foo) ! { dg-error "is polymorphic allocatable" }
    class(*), allocatable :: foo
    foo = foo_t()
  end function

  pure function g2() ! { dg-error "is polymorphic allocatable" }
    class(*), allocatable :: g2
    g2 = foo_t()
  end function

  pure function h() result(bar) ! { dg-error "polymorphic allocatable component" }
    type(bar_t) :: bar
  end function

  pure function h2() ! { dg-error "polymorphic allocatable component" }
    type(bar_t) :: h2
  end function

end
