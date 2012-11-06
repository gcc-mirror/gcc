! { dg-do run }
!
! PR 54917: [OOP] TRANSFER on polymorphic variable causes ICE
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module m
  implicit none
  type test_type
    integer :: i = 0
  contains
    procedure :: ass
    generic :: assignment(=) => ass
  end type
contains
  subroutine ass (a, b)
    class(test_type), intent(out) :: a
    class(test_type), intent(in)  :: b
    a%i = b%i
  end subroutine
end module


program p
  use m
  implicit none

  class(test_type), allocatable :: c
  type(test_type) :: t

  allocate(c)

  ! (1) check CLASS-to-TYPE transfer
  c%i=3
  t = transfer(c, t)
  if (t%i /= 3) call abort()

  ! (2) check TYPE-to-CLASS transfer
  t%i=4
  c = transfer(t, c)
  if (c%i /= 4) call abort()

end

! { dg-final { cleanup-modules "m" } }
