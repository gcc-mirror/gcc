! { dg-do run }
! PR 53800

! Check that a CLASS array with an extended dynamic type passed to an
! assumed-shape TYPE dummy aliases the original storage, rather
! than a copy-in/copy-out temporary that goes stale after return.
!
! Reported by Tobias Burnus  <burnus@gcc.gnu.org>

program class_to_type
  implicit none
  type t
    integer :: i
  end type t
  type, extends(t) :: t2
    integer :: j
  end type t2
  class(t), target, allocatable :: a(:,:)
  type(t), pointer :: ptr

  allocate (t2 :: a(5,5))
  a(:,:)%i = 53
  a(3,3)%i = 42
  a(4,4)%i = 74

  call f (a)
  if (ptr%i /= 42) stop 1
  a(3,3)%i = 999
  if (ptr%i /= 999) stop 2
contains
  subroutine f(x)
    type(t), target :: x(:,:)
    ptr => x(3,3)
  end subroutine f
end program class_to_type
