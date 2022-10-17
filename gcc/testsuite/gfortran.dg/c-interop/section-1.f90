! { dg-do run }
! { dg-additional-sources "section-1-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program tests basic use of the CFI_section C library function on
! a 1-dimensional non-pointer/non-allocatable array, passed as an
! assumed-shape dummy.

program testit
  use iso_c_binding
  implicit none

  interface
    subroutine ctest (a, lb, ub, s, r) bind (c)
      use iso_c_binding
      integer(C_INT), target :: a(:)
      integer(C_INT), value :: lb, ub, s
      integer(C_INT), pointer, intent(out) :: r(:)
    end subroutine

  end interface

  integer(C_INT), target :: aa(32)
  integer :: i

  ! Initialize the test array by numbering its elements.
  do i = 1, 32
    aa(i) = i
  end do

  ! Try some cases with non-pointer input arrays.
  call test (aa, 1, 32, 5, 13, 2)      ! basic test
  call test (aa, 4, 35, 5, 13, 2)      ! non-default lower bound
  call test (aa, 1, 32, 32, 16, -2)    ! negative step

contains

  ! Test function for non-pointer array AA.
  ! LO and HI are the bounds for the entire array.
  ! LB, UB, and S describe the section to take, and use the
  ! same indexing as LO and HI.
  subroutine test (aa, lo, hi, lb, ub, s)
    integer :: aa(lo:hi)
    integer :: lo, hi, lb, ub, s

    integer(C_INT), pointer :: rr(:)
    integer :: i, o

    ! Call the C function to put a section in rr.
    ! The C function expects the section bounds to be 1-based.
    nullify (rr)
    call ctest (aa, lb - lo + 1, ub - lo + 1, s, rr)

    ! Make sure the original array has not been modified.
    do i = lo, hi
      if (aa(i) .ne. i - lo + 1) stop 103
    end do

    ! Make sure the output array has the expected bounds and elements.
    if (.not. associated (rr)) stop 111
    if (lbound (rr, 1) .ne. 1) stop 112
    if (ubound (rr, 1) .ne. (ub - lb)/s + 1) stop 113
    o = 1
    do i = lb, ub, s
      if (rr(o) .ne. i - lo + 1) stop 114
      o = o + 1
    end do
  end subroutine

end program

