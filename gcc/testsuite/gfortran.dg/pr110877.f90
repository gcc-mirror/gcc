! { dg-do run }
!
! PR fortran/110877
! Incorrect copy of allocatable component in polymorphic assignment
! from an array dummy argument.

module pr110877_m
  type :: foo_t
  end type foo_t

  type, extends(foo_t) :: bar_t
    real, allocatable :: a
  end type bar_t
end module pr110877_m

program pr110877
  use pr110877_m
  implicit none

  class(foo_t), allocatable :: foo(:)

  allocate(bar_t :: foo(1))
  select type (foo)
  class is (bar_t)
    allocate(foo(1)%a)
  end select

  call check_assign(foo)

contains

  subroutine check_assign(f)
    class(foo_t), intent(in) :: f(:)
    class(foo_t), allocatable :: g(:)

    g = f
    select type (g)
    class is (bar_t)
      if (.not. allocated(g(1)%a)) stop 1
    end select

    deallocate(g)
    allocate(g, source=f)
    select type (g)
    class is (bar_t)
      if (.not. allocated(g(1)%a)) stop 2
    end select
  end subroutine check_assign
end program pr110877
