! { dg-do run }
! PR fortran/102287 - optional allocatable DT array arguments (intent out)

module m
  type t
     integer, allocatable :: a
  end type t
contains
  subroutine a (x, v)
    type(t), optional, allocatable, intent(out) :: x(:)
    type(t), optional,              intent(out) :: v(:)
    call b (x, v)
  end subroutine a

  subroutine b (y, w)
    type(t), optional, allocatable, intent(out) :: y(:)
    type(t), optional,              intent(out) :: w(:)
  end subroutine b
end module m

program p
  use m
  call a ()
end
