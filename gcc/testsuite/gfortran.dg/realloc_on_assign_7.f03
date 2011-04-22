! { dg-do run }
! Check the fix for PR48462 in which the assignments involving matmul
! seg faulted because a was automatically freed before the assignment.
!
! Contributed by John Nedney  <ortp21@gmail.com>
!
program main
  implicit none
  integer, parameter :: dp = kind(0.0d0)
  real(kind=dp), allocatable :: delta(:,:)
  
  call foo
  call bar
contains
!
! Original reduced version from comment #2
  subroutine foo
    implicit none
    real(kind=dp), allocatable :: a(:,:)
    real(kind=dp), allocatable :: b(:,:)

    allocate(a(3,3))
    allocate(b(3,3))
    allocate(delta(3,3))

    b = reshape ([1d0, 0d0, 0d0, 0d0, 1d0, 0d0, 0d0, 0d0, 1d0], [3,3])
    a = reshape ([1d0, 2d0, 3d0, 4d0, 5d0, 6d0, 7d0, 8d0, 9d0], [3,3])

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
end program main

