! { dg-do run }
!
! Correct an error in the eveluation of the CFI descriptor attribute for
! the case where the bind_C formal argument is not an assumed shape array
! and not allocatable or pointer.
!
! Contributed by Gilles Gouaillardet  <gilles@rist.or.jp>
!
MODULE FOO
INTERFACE
SUBROUTINE dummy(buf) BIND(C, name="clock")
type(*), dimension(..) :: buf
END SUBROUTINE
END INTERFACE
END MODULE

PROGRAM main
    USE FOO
    IMPLICIT NONE
    integer(8) :: before, after

    INTEGER, parameter :: n = 1

    INTEGER, ALLOCATABLE :: buf(:)
    INTEGER :: buf2(n)
    INTEGER :: i

    ALLOCATE(buf(n))
    before = LOC(buf(1))
    CALL dummy (buf)
    after = LOC(buf(1))

    if (before .NE. after) stop 1

    before = LOC(buf2(1))
    CALL dummy (buf)
    after = LOC(buf2(1))

    if (before .NE. after) stop 2

END PROGRAM
