! { dg-do run }
! Check the fix for PR48462 in which the assignments involving matmul
! seg faulted because a was automatically freed before the assignment.
! Since it is related, the test for the fix of PR48746 has been added
! as a subroutine by that name.
!
! Contributed by John Nedney  <ortp21@gmail.com>
!
program main
  implicit none
  integer, parameter :: dp = kind(0.0d0)
  real(kind=dp), allocatable :: delta(:,:)
  real(kind=dp), allocatable, target :: a(:,:)
  real(kind=dp), pointer :: aptr(:,:)

  allocate(a(3,3))
  aptr => a
  
  call foo
  if (.not. associated (aptr, a)) call abort () ! reallocated to same size - remains associated
  call bar
  if (.not. associated (aptr, a)) call abort () ! reallocated to smaller size - remains associated
  call foobar
  if (associated (aptr, a)) call abort () ! reallocated to larger size - disassociates

  call pr48746
contains
!
! Original reduced version from comment #2
  subroutine foo
    implicit none
    real(kind=dp), allocatable :: b(:,:)

    allocate(b(3,3))
    allocate(delta(3,3))

    a = reshape ([1d0, 2d0, 3d0, 4d0, 5d0, 6d0, 7d0, 8d0, 9d0], [3,3])
    b = reshape ([1d0, 0d0, 0d0, 0d0, 1d0, 0d0, 0d0, 0d0, 1d0], [3,3])

    a = matmul( matmul( a, b ), b )
    delta = (a - reshape ([1d0, 2d0, 3d0, 4d0, 5d0, 6d0, 7d0, 8d0, 9d0], [3,3]))**2
    if (any (delta > 1d-12)) call abort
    if (any (lbound (a) .ne. [1, 1])) call abort
  end subroutine
!
! Check that all is well when the shape of 'a' changes.
  subroutine bar
    implicit none
    real(kind=dp), allocatable :: a(:,:)
    real(kind=dp), allocatable :: b(:,:)

    b = reshape ([1d0, 1d0, 1d0], [3,1])
    a = reshape ([1d0, 2d0, 3d0, 4d0, 5d0, 6d0, 7d0, 8d0, 9d0], [3,3])

    a = matmul( a, matmul( a, b ) )

    delta = (a - reshape ([198d0, 243d0, 288d0], [3,1]))**2
    if (any (delta > 1d-12)) call abort
    if (any (lbound (a) .ne. [1, 1])) call abort
  end subroutine
  subroutine foobar
    integer :: i
    a = reshape ([(real(i, dp), i = 1, 100)],[10,10])
  end subroutine
  subroutine pr48746
! This is a further wrinkle on the original problem and came about
! because the dtype field of the result argument, passed to matmul,
! was not being set. This is needed by matmul for the rank.
!
! Contributed by Thomas Koenig  <tkoenig@gcc.gnu.org>
!
    implicit none
    integer, parameter :: m=10, n=12, count=4
    real :: optmatmul(m, n)
    real :: a(m, count), b(count, n), c(m, n)
    real, dimension(:,:), allocatable :: tmp
    call random_number(a)
    call random_number(b)
    tmp = matmul(a,b)
    if (any (lbound (tmp) .ne. [1,1])) call abort
    if (any (ubound (tmp) .ne. [10,12])) call abort
  end subroutine
end program main

