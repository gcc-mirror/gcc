! { dg-do run }
!
! Tests that PR63932 stays fixed.
!
! Contributed by Valery Weber  <valeryweber@hotmail.com>
!
module mod
  type :: t
     character(:), allocatable :: c
     integer :: i
   contains
     procedure, pass :: get
  end type t
  type :: u
     character(:), allocatable :: c
  end type u
contains
  subroutine get(this, a)
    class(t), intent(in) :: this
    character(:), allocatable, intent(out), optional :: a
    if (present (a)) a = this%c
  end subroutine get
end module mod

program test
  use mod
  type(t) :: a
  type(u) :: b
  a%c = 'something'
  call a%get (a = b%c)
  if (b%c .ne. 'something') STOP 1
end program test
