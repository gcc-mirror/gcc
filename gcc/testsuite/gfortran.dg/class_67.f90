! { dg-do run }
!
! Test the fix for PR78990 in which the scalarization of the assignment
! in the main program failed for two reasons: (i) The conversion of 'v1'
! into a class actual was being done after the call to 'return_t1', giving
! rise to the ICE reported in comment #1; and (ii) The 'info' descriptor,
! required for scalarization was not set, which gave rise to the ICE noted
! by the contributor.
!
! Contributed by Chris Macmackin  <cmacmackin@gmail.com>
!
module test_type
  implicit none

  type t1
     integer :: i
   contains
     procedure :: assign
     generic :: assignment(=) => assign
  end type t1

contains

  elemental subroutine assign(this,rhs)
    class(t1), intent(inout) :: this
    class(t1), intent(in) :: rhs
    this%i = rhs%i
  end subroutine assign

  function return_t1(arg)
    class(t1), dimension(:), intent(in) :: arg
    class(t1), dimension(:), allocatable :: return_t1
    allocate(return_t1(size(arg)), source=arg)
  end function return_t1

  function return_t1_p(arg)
    class(t1), dimension(:), intent(in), target :: arg
    class(t1), dimension(:), pointer :: return_t1_p
    return_t1_p => arg
  end function return_t1_p
end module test_type

program test
  use test_type
  implicit none

  type(t1), dimension(3) :: v1, v2
  v1%i = [1,2,3]
  v2 = return_t1(v1)
  if (any (v2%i .ne. v1%i)) STOP 1

  v1%i = [4,5,6]
  v2 = return_t1_p(v1)
  if (any (v2%i .ne. v1%i)) STOP 2
end program test
