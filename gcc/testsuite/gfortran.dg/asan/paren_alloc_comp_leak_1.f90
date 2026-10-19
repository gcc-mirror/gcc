! { dg-do run }
!
! PR 128357
! A parenthesized variable of a type with allocatable components, passed as
! an actual argument or used as an ASSOCIATE selector, leaked the array
! temporary or the components of its copies.  Checked under
! -fsanitize=address.

module m
  implicit none
  type :: t
    integer :: c = 7
    integer, allocatable :: v(:)
  end type
contains
  subroutine s (x)
    type(t), intent(in) :: x(:)
    if (x(2)%v(2) /= 2) stop 1
  end subroutine
  subroutine s1 (x)
    type(t), intent(in) :: x
    if (x%v(2) /= 2) stop 2
  end subroutine
  subroutine sc (x)
    class(t), intent(in) :: x(:)
    if (x(2)%v(2) /= 2) stop 3
  end subroutine
  ! The temporaries live only for the call, so they leak on return.
  subroutine work ()
    type(t), target :: ta(2)
    class(t), pointer :: qa(:)
    class(t), allocatable :: ca(:)

    ta(1) = t (1, [1])
    ta(2) = t (2, [2, 2])
    qa => ta
    allocate (ca, source = ta)

    call s ((ta))
    call s1 ((ta(2)))
    call sc ((qa))
    call sc ((ca))
    if (any (ta(2)%v /= 2) .or. any (ca(2)%v /= 2)) stop 4

    associate (b => (qa))
      if (any (b%c /= [1, 2])) stop 5
    end associate
  end subroutine
end module

program p
  use m
  implicit none
  call work ()
end program
