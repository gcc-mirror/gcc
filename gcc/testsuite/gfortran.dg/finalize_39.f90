! { dg-do run }
!
! Test the fix for PR67444 in which the finalization of a polymorphic 'var'
! was not being finalized before assignment. (STOP 3)
!
! Contributed by Balint Aradi  <baladi@gmail.com>
!
module classes
  implicit none
  integer :: ivalue = 0
  integer :: icall = 0
  integer :: fvalue = 0

  type :: Basic
    integer :: ii = -1
  contains
    procedure :: assignBasic
    generic :: assignment(=) => assignBasic
    final :: destructBasic
  end type Basic
  interface Basic
    module procedure initBasic
  end interface Basic
contains
  function initBasic(initValue) result(this)
    integer, intent(in) :: initValue
    type(Basic) :: this
    this%ii = initValue
    icall = icall + 1
  end function initBasic
  subroutine assignBasic(this, other)
    class(Basic), intent(out) :: this
    type(Basic), intent(in) :: other
    this%ii = other%ii + 1
    icall = other%ii
  end subroutine assignBasic
  subroutine destructBasic(this)
    type(Basic), intent(inout) :: this
    fvalue = fvalue + 1
    select case (fvalue)
    case (1)
        if (this%ii /= -1) stop 1          ! First finalization before assignment to 'var'
        if (icall /= 1) stop 2             ! and before evaluation of 'expr'.
    case(2)
        if (this%ii /= ivalue) stop 3      ! Finalization of intent(out) in 'assignBasic'
        if (icall /= 42) stop 4            ! and after evaluation of 'expr'.
    case(3)
        if (this%ii /= ivalue + 1) stop 5  ! Finalization of 'expr' (function!) after assignment.
    case default
        stop 6                             ! Too many or no finalizations
    end select
  end subroutine destructBasic
end module classes

module usage
  use classes
  implicit none
contains
  subroutine useBasic()
    type(Basic) :: bas
    ivalue = 42
    bas = Basic(ivalue)
  end subroutine useBasic
end module usage

program test
  use usage
  implicit none
  call useBasic()
  if (fvalue /= 3) stop 7                  ! 3 finalizations mandated.
end program test
