! { dg-do run }
!
! TS 29113
! 6.3  Argument association
! An assumed-rank dummy argument may correspond to an actual argument of
! any rank. [...] If the actual argument has rank greater than zero, the
! rank and extents of the dummy argument are assumed from the actual
! argument, including the lack of a final extent in the case of an
! assumed-size array. If the actual argument is an array and the dummy
! argument is allocatable or a pointer, the bounds of the dummy argument
! are assumed from the actual argument.

program test 

  integer, allocatable :: a(:,:,:)
  integer, allocatable :: b(:,:,:,:)

  allocate (a(3, 4, 5))
  allocate (b(-3:3, 0:4, 2:5, 10:20))

  call testit (a, rank(a), shape(a), lbound(a), ubound(a))
  call testit (b, rank(b), shape(b), lbound(b), ubound(b))

contains

  subroutine testit (x, r, s, l, u)
    integer, allocatable :: x(..)
    integer :: r
    integer :: s(r)
    integer :: l(r)
    integer :: u(r)

    ! expect rank to match
    if (rank (x) .ne. r) stop 101

    ! expect shape to match
    if (size (shape (x)) .ne. r) stop 102
    if (any (shape (x) .ne. s))  stop 103

    ! expect lbound and ubound functions to return rank-sized arrays.
    ! for non-pointer/non-allocatable arrays, bounds are normalized
    ! to be 1-based. 
    if (size (lbound (x)) .ne. r) stop 104
    if (any (lbound (x) .ne. l)) stop 105

    if (size (ubound (x)) .ne. r) stop 106
    if (any (ubound (x) .ne. u)) stop 107
  end subroutine

end program
