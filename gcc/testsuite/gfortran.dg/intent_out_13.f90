! { dg-do run }
! PR 88364 -- too much was clobbered on call.
module pr88364
  implicit none
  type t
    integer :: b = -1
    integer :: c = 2
  end type t
contains
  subroutine f1 (x)
    integer, intent(out) :: x
    x = 5
  end subroutine f1
  subroutine f2 ()
    type(t) :: x
    call f1 (x%b)
    if (x%b .ne. 5 .or. x%c .ne. 2) stop 1
  end subroutine f2
end module pr88364
  use pr88364
  call f2
end
