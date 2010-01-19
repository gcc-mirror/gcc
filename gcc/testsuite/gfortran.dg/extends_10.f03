! { dg-do compile }
!
! PR 42545: type extension: parent component has wrong accessibility
!
! Reported by Reinhold Bader <bader@lrz.de>

module mo
  implicit none
  type :: t1
    integer :: i = 1
  end type
  type, extends(t1) :: t2
    private
    real :: x = 2.0
  end type
  type :: u1
    integer :: j = 1
  end type
  type, extends(u1) :: u2
    real :: y = 2.0
  end type
  private :: u1
end module

program pr
  use mo
  implicit none
  type(t2) :: a
  type(u2) :: b
  print *,a%t1%i
  print *,b%u1%j  ! { dg-error "is a PRIVATE component of" }
end program

! { dg-final { cleanup-modules "mo" } }
