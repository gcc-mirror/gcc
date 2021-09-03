! { dg-do run }
!
! TS 29113
! 6.3  Argument association
! An assumed-rank dummy argument may correspond to an actual argument of
! any rank. If the actual argument has rank zero, the dummy argument has
! rank zero; the shape is a zero-sized array and the LBOUND and UBOUND
! intrinsic functions, with no DIM argument, return zero-sized
! arrays.  [...]

program test 

  call testit (42)

contains

  subroutine testit (x0)
    integer :: x0(..)

    ! expect to have rank 0
    if (rank (x0) .ne. 0) stop 101

    ! expect shape to be a zero-sized array
    if (size (shape (x0)) .ne. 0) stop 102

    ! expect lbound and ubound functions to return zero-sized arrays
    if (size (lbound (x0)) .ne. 0) stop 103
    if (size (ubound (x0)) .ne. 0) stop 104
  end subroutine

end program
