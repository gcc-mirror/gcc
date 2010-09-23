! { dg-do compile }
!
! PR 43169: [OOP] gfortran rejects PURE procedure with SELECT TYPE construct
!
! Original test case by Todd Hay <haymaker@mail.utexas.edu>
! Modified by Janus Weil <janus@gcc.gnu.org>

  implicit none
  real :: g

contains

  pure subroutine sub1(x)
    type :: myType
      real :: a
    end type myType
    class(myType), intent(inout) :: x
    real :: r3
    select type(x)
    class is (myType)
      x%a = 42.
      r3 =  43.
      g = 44.             ! { dg-error "variable definition context" }
    end select
  end subroutine

  pure subroutine sub2
    real :: r1
    block
      real :: r2
      r1 = 45.
      r2 = 46.
      g = 47.             ! { dg-error "variable definition context" }
    end block
  end subroutine

  pure subroutine sub3
    block
      integer, save :: i  ! { dg-error "cannot be specified in a PURE procedure" }
      integer :: j = 5    ! { dg-error "is not allowed in a PURE procedure" }
    end block
  end subroutine

end
