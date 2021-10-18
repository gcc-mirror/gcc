! { dg-do compile}
!
! TS 29113
! C407a An assumed-type entity shall be a dummy variable that does not 
! have the ALLOCATABLE, CODIMENSION, INTENT(OUT), POINTER, or VALUE 
! attribute and is not an explicit-shape array.
!
! This test file contains tests that are expected to all pass.

! Check basic usage with no attributes.

module m
  interface
    subroutine g (a, b)
      implicit none
      type(*) :: a
      integer :: b
    end subroutine
  end interface
end module

subroutine s0 (x)
  use m
  implicit none
  type(*) :: x

  call g (x, 1)
end subroutine

! Check that other attributes that can normally apply to dummy variables
! are allowed.

subroutine s1 (a, b, c, d, e, f, g, h)
  implicit none
  type(*), asynchronous :: a
  type(*), contiguous :: b(:,:)
  type(*), dimension (:) :: c
  type(*), intent(in) :: d
  type(*), intent(inout) :: e
  type(*), optional :: f
  type(*), target :: g
  type(*), volatile :: h

end subroutine

! Check that non-explicit-shape arrays are allowed.

subroutine s2 (a, b, c)
  implicit none
  type(*) :: a(:)  ! assumed-shape
  type(*) :: b(*)  ! assumed-size
  type(*) :: c(..) ! assumed-rank

end subroutine

