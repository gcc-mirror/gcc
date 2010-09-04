! { dg-do run }
! 
! PR fortran/45019
!
! Check that the compiler knows that
! "arg" and "arr" can alias.
!
MODULE m
  IMPLICIT NONE
  INTEGER, TARGET :: arr(3)
CONTAINS
  SUBROUTINE foobar (arg)
    INTEGER, TARGET :: arg(:)
    arr(2:3) = arg(1:2)
  END SUBROUTINE foobar
END MODULE m

PROGRAM main
  USE m
  IMPLICIT NONE
  arr = (/ 1, 2, 3 /)
  CALL bar(arr)
  if (any (arr /= (/ 1, 1, 2 /))) call abort()
  CALL test()
contains
  subroutine bar(x)
    INTEGER, TARGET :: x(:)
    CALL foobar (x)
  end subroutine bar
END PROGRAM main

MODULE m2
  IMPLICIT NONE
  INTEGER, TARGET :: arr(3)
CONTAINS
   SUBROUTINE foobar (arg)
    INTEGER, TARGET :: arg(:)
    arr(1) = 5
    arg(1) = 6
    if (arr(1) == 5) call abort()
  END SUBROUTINE foobar
END MODULE m2
subroutine test
  USE m2
  IMPLICIT NONE
  arr = (/ 1, 2, 3 /)
  CALL bar(arr)
contains
   subroutine bar(x)
    INTEGER, TARGET :: x(:)
    CALL foobar (x)
  end subroutine bar
END subroutine test

! { dg-final { cleanup-modules "m m2" } }
