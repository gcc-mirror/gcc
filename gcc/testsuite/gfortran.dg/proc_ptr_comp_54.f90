!{ dg-do run }

! Do not free procedure pointer components.
! Contributed by Damian Rouson  <damian@archaeologic.codes>

  implicit none

  type foo_t
    integer, allocatable :: i_
    procedure(f), pointer, nopass :: f_
    procedure(c), pointer, nopass :: c_
  end type

  class(foo_t), allocatable :: ff

  associate(foo => foo_t(1,f))
  end associate

contains

  function f()
    logical, allocatable :: f
    f = .true.
  end function

  function c()
    class(foo_t), allocatable :: c
    allocate(c)
  end function
end
