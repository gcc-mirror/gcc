! { dg-do run }
!
! Test the fix for PR94331
!
! Contributed by Igor Gayday <igor.gayday@mu.edu>
!

MODULE FOO

  implicit none
  
  INTEGER, parameter :: n = 11

contains
  
  SUBROUTINE dummyc(x0) BIND(C)
    type(*), dimension(..) :: x0
    if(LBOUND(x0,1)/=1) stop 5
    if(UBOUND(x0,1)/=n) stop 6
    if(rank(x0)/=1)     stop 7
  END SUBROUTINE dummyc
  
  SUBROUTINE dummy(x0)
    type(*), dimension(..) :: x0
    call dummyc(x0)
  END SUBROUTINE dummy
  
END MODULE

PROGRAM main
    USE FOO
    IMPLICIT NONE
    integer :: before(2), after(2)

    DOUBLE PRECISION, ALLOCATABLE :: buf(:)
    DOUBLE PRECISION :: buf2(n)

    ALLOCATE(buf(n))
    before(1) = LBOUND(buf,1)
    before(2) = UBOUND(buf,1)
    CALL dummy (buf)
    after(1) = LBOUND(buf,1)
    after(2) = UBOUND(buf,1)
    deallocate(buf)

    if (before(1) .NE. after(1)) stop 1
    if (before(2) .NE. after(2)) stop 2

    before(1) = LBOUND(buf2,1)
    before(2) = UBOUND(buf2,1)
    CALL dummy (buf2)
    after(1) = LBOUND(buf2,1)
    after(2) = UBOUND(buf2,1)

    if (before(1) .NE. after(1)) stop 3
    if (before(2) .NE. after(2)) stop 4

END PROGRAM
