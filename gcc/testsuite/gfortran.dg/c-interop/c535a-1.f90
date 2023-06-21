! { dg-do compile }
!
! TS 29113
! C535a  An assumed-rank entity shall be a dummy variable that does not
! have the CODIMENSION or VALUE attribute.
! An assumed-rank object may have the CONTIGUOUS attribute.
!
! This test file contains tests that are expected to all pass.

! Check basic usage with no attributes.

module m
  type :: t
    integer :: i
    real :: f
  end type
end module

subroutine s0 (a, b, c, d)
  use m
  implicit none
  integer :: a(..)
  real :: b(..)
  type(t) :: c(..)
  type(*) :: d(..)
end subroutine

! Likewise with dimension attribute.

subroutine s1 (a, b, c, d)
  use m
  implicit none
  integer, dimension(..) :: a
  real, dimension(..) :: b
  type(t), dimension(..) :: c
  type(*), dimension(..) :: d
end subroutine

! Likewise with dimension statement.

subroutine s2 (a, b, c, d)
  use m
  implicit none
  integer :: a
  real :: b
  type(t) :: c
  type(*) :: d
  dimension a(..), b(..), c(..), d(..)
end subroutine

! Test that various other attributes are accepted.

subroutine s3 (a, b, c, d, e, f, g, h, i, j)
  implicit none
  integer, allocatable :: a(..)
  integer, asynchronous :: b(..)
  integer, contiguous :: c(..)
  integer, intent(in) :: d(..)
  integer, intent(out) :: e(..)
  integer, intent(inout) :: f(..)
  integer, optional :: g(..)
  integer, pointer :: h(..)
  integer, target :: i(..)
  integer, volatile :: j(..)
end subroutine
