! { dg-do compile}
!
! TS 29113
! C407b  An assumed-type variable name shall not appear in a designator
! or expression except as an actual argument corresponding to a dummy
! argument that is assumed-type, or as the first argument to any of
! the intrinsic and intrinsic module functions IS_CONTIGUOUS, LBOUND, 
! PRESENT, RANK, SHAPE, SIZE, UBOUND, and C_LOC.
!
! This test file contains tests that are expected to all pass.

! Check that passing an assumed-type variable as an actual argument 
! corresponding to an assumed-type dummy works.

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

! Check that calls to the permitted intrinsic functions work.

function test_is_contiguous (a)
  implicit none
  type(*) :: a(*)
  logical :: test_is_contiguous

  test_is_contiguous = is_contiguous (a)
end function

function test_lbound (a)
  implicit none
  type(*) :: a(:)
  integer :: test_lbound

  test_lbound = lbound (a, 1)
end function

function test_present (a)
  implicit none
  type(*), optional :: a(*)
  logical :: test_present

  test_present = present (a)
end function

function test_rank (a)
  implicit none
  type(*) :: a(*)
  integer :: test_rank

  test_rank = rank (a)
end function

function test_shape (a)
  implicit none
  type(*) :: a(:)  ! assumed-shape array so shape intrinsic works
  integer :: test_shape

  integer :: temp, i
  integer, dimension (rank (a)) :: ashape

  temp = 1
  ashape = shape (a)
  do i = 1, rank (a)
    temp = temp * ashape (i)
  end do
  test_shape = temp
end function

function test_size (a)
  implicit none
  type(*) :: a(:)
  integer :: test_size

  test_size = size (a)
end function

function test_ubound (a)
  implicit none
  type(*) :: a(:)
  integer :: test_ubound

  test_ubound = ubound (a, 1)
end function

function test_c_loc (a)
  use iso_c_binding
  implicit none
  type(*), target :: a(*)
  type(c_ptr) :: test_c_loc

  test_c_loc = c_loc (a)
end function

