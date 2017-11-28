! { dg-do run }
!
! Test the fix for PR69739 in which the statement
! R = operate(A, X) caused an ICE.
!
! Contributed by John  <jwmwalrus@gmail.com>
!
module test

  implicit none
  type, public :: sometype
    real :: a    =  0.
  end type
contains

  function dosomething(A) result(r)
    type(sometype), intent(IN) :: A(:,:,:)
    integer :: N
    real, allocatable ::   R(:), X(:)

    N = PRODUCT(UBOUND(A))
    allocate (R(N),X(N))
    X = [(real(N), N = 1, size(X, 1))]
    R = operate(A, X)
  end function

  function operate(A, X)
    type(sometype), intent(IN) :: A(:,:,:)
    real, intent(IN) :: X(:)
    real :: operate(1:PRODUCT(UBOUND(A)))

    operate = x
  end function
end module test

  use test
  type(sometype) :: a(2, 2, 2)
  if (any(int (dosomething(a)) .ne. [1,2,3,4,5,6])) call abort
end
