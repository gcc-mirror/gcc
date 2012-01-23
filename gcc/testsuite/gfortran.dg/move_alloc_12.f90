! { dg-do compile }
!
! PR fortran/51948
!
  type :: t
  end type t
contains
  function func(x, y)
    class(t) :: y
    type(t), allocatable :: func
    type(t), allocatable :: x

    select type (y)
      type is(t)
        call move_alloc (x, func)
    end select
  end function

  function func2(x, y)
    class(t) :: y
    class(t), allocatable :: func2
    class(t), allocatable :: x

    block
    block
    select type (y)
      type is(t)
        call move_alloc (x, func2)
    end select
    end block
    end block
  end function
end
