! { dg-do compile}
! { dg-additional-options "-fcoarray=single" }
!
! TS 29113
! C535b An assumed-rank variable name shall not appear in a designator
! or expression except as an actual argument corresponding to a dummy 
! argument that is assumed-rank, the argument of the C_LOC function
! in the ISO_C_BINDING intrinsic module, or the first argument in a
! reference to an intrinsic inquiry function.
!
! This has been renamed C838 in the Fortran 2018 standard, with C_SIZEOF
! and SELECT_RANK additionally added.
!
! This test file contains tests that are expected to all pass.

! Check that passing an assumed-rank variable as an actual argument 
! corresponding to an assumed-rank dummy works.

module m
  interface
    subroutine g (a, b)
      implicit none
      real :: a(..)
      integer :: b
    end subroutine
  end interface
end module

subroutine s0 (x)
  use m
  implicit none
  real :: x(..)

  call g (x, 1)
end subroutine

! Check that calls to the permitted intrinsic functions work.

function test_c_loc (a)
  use iso_c_binding
  implicit none
  integer, target :: a(..)
  type(c_ptr) :: test_c_loc

  test_c_loc = c_loc (a)
end function

function test_allocated (a)
  implicit none
  integer, allocatable :: a(..)
  logical :: test_allocated

  test_allocated = allocated (a)
end function

! 2-argument forms of the associated intrinsic are tested in c535b-3.f90.
function test_associated (a)
  implicit none
  integer, pointer :: a(..)
  logical :: test_associated

  test_associated = associated (a)
end function

function test_bit_size (a)
  implicit none
  integer :: a(..)
  integer :: test_bit_size

  test_bit_size = bit_size (a)
end function

function test_digits (a)
  implicit none
  integer :: a(..)
  integer :: test_digits

  test_digits = digits (a)
end function

function test_epsilon (a)
  implicit none
  real :: a(..)
  real :: test_epsilon

  test_epsilon = epsilon (a)
end function

function test_huge (a)
  implicit none
  integer :: a(..)
  integer :: test_huge

  test_huge = huge (a)
end function

function test_is_contiguous (a)
  implicit none
  integer :: a(..)
  logical :: test_is_contiguous

  test_is_contiguous = is_contiguous (a)
end function

function test_kind (a)
  implicit none
  integer :: a(..)
  integer :: test_kind

  test_kind = kind (a)
end function

function test_lbound (a)
  implicit none
  integer :: a(..)
  integer :: test_lbound

  test_lbound = lbound (a, 1)
end function

function test_len1 (a)
  implicit none
  character(len=5) :: a(..)
  integer :: test_len1

  test_len1 = len (a)
end function

function test_len2 (a)
  implicit none
  character(len=*) :: a(..)
  integer :: test_len2

  test_len2 = len (a)
end function

function test_len3 (a)
  implicit none
  character(len=5), pointer :: a(..)
  integer :: test_len3

  test_len3 = len (a)
end function

function test_len4 (a)
  implicit none
  character(len=*), pointer :: a(..)
  integer :: test_len4

  test_len4 = len (a)
end function

function test_len5 (a)
  implicit none
  character(len=:), pointer :: a(..)
  integer :: test_len5

  test_len5 = len (a)
end function

function test_len6 (a)
  implicit none
  character(len=5), allocatable :: a(..)
  integer :: test_len6

  test_len6 = len (a)
end function

function test_len7 (a)
  implicit none
  character(len=*), allocatable :: a(..)
  integer :: test_len7

  test_len7 = len (a)
end function

function test_len8 (a)
  implicit none
  character(len=:), allocatable :: a(..)
  integer :: test_len8

  test_len8 = len (a)
end function

function test_maxexponent (a)
  implicit none
  real :: a(..)
  integer :: test_maxexponent

  test_maxexponent = maxexponent (a)
end function

function test_minexponent (a)
  implicit none
  real :: a(..)
  integer :: test_minexponent

  test_minexponent = minexponent (a)
end function

function test_new_line (a)
  implicit none
  character :: a(..)
  character :: test_new_line

  test_new_line = new_line (a)
end function

function test_precision (a)
  implicit none
  real :: a(..)
  integer :: test_precision

  test_precision = precision (a)
end function

function test_present (a, b, c)
  implicit none
  integer :: a, b
  integer, optional :: c(..)
  integer :: test_present

  if (present (c)) then
    test_present = a
  else
    test_present = b
  end if
end function

function test_radix (a)
  implicit none
  real :: a(..)
  integer :: test_radix

  test_radix = radix (a)
end function

function test_range (a)
  implicit none
  real :: a(..)
  integer :: test_range

  test_range = range (a)
end function

function test_rank (a)
  implicit none
  integer :: a(..)
  integer :: test_rank

  test_rank = rank (a)
end function

function test_shape (a)
  implicit none
  integer :: a(..)
  logical :: test_shape

  test_shape = (rank (a) .eq. size (shape (a)))
end function

function test_size (a)
  implicit none
  integer :: a(..)
  logical :: test_size

  test_size = (size (a) .eq. product (shape (a)))
end function

function test_storage_size (a)
  implicit none
  integer :: a(..)
  integer :: test_storage_size

  test_storage_size = storage_size (a)
end function

function test_tiny (a)
  implicit none
  real :: a(..)
  real :: test_tiny

  test_tiny = tiny (a)
end function

function test_ubound (a)
  implicit none
  integer :: a(..)
  integer :: test_ubound

  test_ubound = ubound (a, 1)
end function

! Note:  there are no tests for these inquiry functions that can't
! take an assumed-rank array argument for other reasons:
!
! coshape, lcobound, ucobound: requires CODIMENSION attribute, which is
!   not permitted on an assumed-rank variable.
!

! F2018 additionally permits the first arg to C_SIZEOF to be
! assumed-rank (C838).

function test_c_sizeof (a)
  use iso_c_binding
  implicit none
  integer :: a(..)
  integer :: test_c_sizeof

  test_c_sizeof = c_sizeof (a)
end function

! F2018 additionally permits an assumed-rank array as the selector
! in a SELECT RANK construct (C838).

function test_select_rank (a)
  implicit none
  integer :: a(..)
  integer :: test_select_rank

  select rank (a)
    rank (0)
      test_select_rank = 0
    rank (1)
      test_select_rank = 1
    rank (2)
      test_select_rank = 2
    rank default
      test_select_rank = -1
  end select
end function
