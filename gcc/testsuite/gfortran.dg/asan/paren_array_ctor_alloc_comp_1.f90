! { dg-do run }
!
! PR 128509
! The components of a parenthesized array constructor of a type with
! allocatable components were freed after the call while still shared
! with TA, which then freed them again.  Checked under -fsanitize=address.

module m
  implicit none
  type :: t
    integer, allocatable :: v(:)
  end type
contains
  subroutine s (x)
    type(t), intent(in) :: x(:)
    if (x(2)%v(2) /= 2) stop 1
  end subroutine
  elemental subroutine se (x)
    type(t), intent(in) :: x
  end subroutine
  subroutine work ()
    type(t) :: ta(2)

    ta(1) = t ([1])
    ta(2) = t ([2, 2])
    call s (([ta]))
    call se (([ta]))
    if (any (ta(2)%v /= 2)) stop 2
  end subroutine
end module

program p
  use m
  implicit none
  call work ()
end program
