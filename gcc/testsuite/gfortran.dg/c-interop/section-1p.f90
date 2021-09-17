! PR 101310
! { dg-do run }
! { dg-additional-sources "section-1-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program tests basic use of the CFI_section C library function on
! a 1-dimensional pointer array.

program testit
  use iso_c_binding
  implicit none

  interface
    subroutine ctest (p, lb, ub, s, r) bind (c)
      use iso_c_binding
      integer(C_INT), pointer :: p(:)
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

  call test_p (aa, 0, 31, 15, 24, 3)   ! zero lower bound
  call test_p (aa, 1, 32, 16, 25, 3)   ! non-zero lower bound
  call test_p (aa, 4, 35, 16, 25, 3)   ! some other lower bound
  call test_p (aa, 1, 32, 32, 16, -2)  ! negative step
  stop

contains

  ! Test function for non-pointer array AA.
  ! LO and HI are the bounds for the entire array.
  ! LB, UB, and S describe the section to take, and use the
  ! same indexing as LO and HI.
  subroutine test_p (aa, lo, hi, lb, ub, s)
    integer, target :: aa(1:hi-lo+1)
    integer :: lo, hi, lb, ub, s

    integer(C_INT), pointer :: pp(:), rr(:)
    integer :: i, o

    pp(lo:hi) => aa
    if (lbound (pp, 1) .ne. lo) stop 121
    if (ubound (pp, 1) .ne. hi) stop 122
    nullify (rr)
    call ctest (pp, lb, ub, s, rr)

    ! Make sure the input pointer array has not been modified.
    if (lbound (pp, 1) .ne. lo) stop 144
    if (ubound (pp, 1) .ne. hi) stop 145
    do i = lo, hi
      if (pp(i) .ne. i - lo + 1) stop 146
    end do

    ! Make sure the output array has the expected bounds and elements.
    if (.not. associated (rr)) stop 151
    if (lbound (rr, 1) .ne. 1) stop 152
    if (ubound (rr, 1) .ne. (ub - lb)/s + 1) stop 153
    o = 1
    do i = lb, ub, s
      if (rr(o) .ne. i - lo + 1) stop 154
      o = o + 1
    end do
  end subroutine

end program

