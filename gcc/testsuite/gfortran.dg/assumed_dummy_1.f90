! { dg-do run }
! Tests the fix for PRs 19358, 19477, 21211 and 21622.
!
! Note that this tests only the valid cases with explicit interfaces.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
module global
contains
  SUBROUTINE goo (x, i)
    REAL, DIMENSION(i:)     :: x
    integer                 :: i
    x (3) = 99.0
  END SUBROUTINE goo
end module global

SUBROUTINE foo (x, i)
  REAL, DIMENSION(i:)       :: x
  integer                   :: i
  x (4) = 42.0
END SUBROUTINE foo

program test
  use global
  real, dimension(3)        :: y = 0
  integer                   :: j = 2

interface
  SUBROUTINE foo (x, i)
    REAL, DIMENSION(i:)     :: x
    integer                 :: i
  END SUBROUTINE foo
end interface
  call foo (y, j)
  call goo (y, j)
  call roo (y, j)
  if (any(y.ne.(/21.0, 99.0, 42.0/))) call abort ()
contains
  SUBROUTINE roo (x, i)
    REAL, DIMENSION(i:)     :: x
    integer                 :: i
    x (2) = 21.0
  END SUBROUTINE roo
end program test

! { dg-final { cleanup-modules "global" } }
