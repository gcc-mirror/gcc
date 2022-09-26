! { dg-do run }
!
! Test the fix for PR100132
!

module main_m
  implicit none

  private

  public :: &
    foo_t

  public :: &
    set,    &
    get

  type :: foo_t
    integer :: i
  end type foo_t

  type(foo_t), save, pointer :: data => null()

contains

  subroutine set(this)
    class(foo_t), pointer, intent(in) :: this

    if(associated(data)) stop 1
    data => this
  end subroutine set

  subroutine get(this)
    type(foo_t), pointer, intent(out) :: this

    if(.not.associated(data)) stop 4
    this => data
    nullify(data)
  end subroutine get

end module main_m

program main_p

  use :: main_m, only: &
    foo_t, set, get

  implicit none

  integer, parameter :: n = 1000

  type(foo_t), pointer :: ps
  type(foo_t),  target :: s
  integer              :: i, j, yay, nay

  yay = 0
  nay = 0
  do i = 1, n
    s%i = i
    call set(s)
    call get(ps)
    if(.not.associated(ps)) stop 13
    j = ps%i
    if(i/=j) stop 14
    if(i/=s%i) stop 15
    if(ps%i/=s%i) stop 16
    if(associated(ps, s))then
      yay = yay + 1
    else
      nay = nay + 1
    end if
  end do
  if((yay/=n).or.(nay/=0)) stop 17

end program main_p
