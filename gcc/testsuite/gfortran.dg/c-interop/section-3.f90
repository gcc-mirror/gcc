! PR 101310
! { dg-do run }
! { dg-additional-sources "section-3-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program tests basic use of the CFI_section C library function to
! take a slice of a 2-dimensional non-pointer array.

module mm
  use ISO_C_BINDING
  type, bind (c) :: m
    integer(C_INT) :: x, y
  end type
end module

program testit
  use iso_c_binding
  use mm
  implicit none

  interface
    subroutine ctest (a, lb0, lb1, ub0, ub1, s0, s1, r) bind (c)
      use iso_c_binding
      use mm
      type(m), target :: a(:,:)
      integer(C_INT), value :: lb0, lb1, ub0, ub1, s0, s1
      type(m), pointer, intent(out) :: r(:)
    end subroutine

  end interface

  type(m), target :: aa(10, 20)
  integer :: i0, i1

  ! Initialize the test array by numbering its elements.
  do i1 = 1, 20
    do i0 = 1, 10
      aa(i0, i1)%x = i0
      aa(i0, i1)%y = i1
    end do
  end do

  call test (aa, 3, 1, 3, 20, 0, 1)        ! full slice 0
  call test (aa, 1, 8, 10, 8, 1, 0)        ! full slice 1
  call test (aa, 3, 5, 3, 14, 0, 3)        ! partial slice 0
  call test (aa, 2, 8, 10, 8, 2, 0)        ! partial slice 1
  call test (aa, 3, 14, 3, 5, 0, -3)       ! backwards slice 0
  call test (aa, 10, 8, 2, 8, -2, 0)       ! backwards slice 1

contains

  ! Test function for non-pointer array AA.
  ! LB, UB, and S describe the section to take.
  subroutine test (aa, lb0, lb1, ub0, ub1, s0, s1)
    use mm
    type(m) :: aa(10,20)
    integer :: lb0, lb1, ub0, ub1, s0, s1

    type(m), pointer :: rr(:)
    integer :: i0, i1, o0, o1
    integer, parameter :: hi0 = 10
    integer, parameter :: hi1 = 20

    ! Check the bounds actually specify a "slice" rather than a subarray.
    if (lb0 .ne. ub0 .and. lb1 .ne. ub1)  stop 100

    ! Call the C function to put a section in rr.
    ! The C function expects the section bounds to be 1-based.
    nullify (rr)
    call ctest (aa, lb0, lb1, ub0, ub1, s0, s1, rr)

    ! Make sure the original array has not been modified.
    do i1 = 1, hi1
      do i0 = 1, hi0
        if (aa(i0,i1)%x .ne. i0) stop 103
        if (aa(i0,i1)%y .ne. i1) stop 103
      end do
    end do

    ! Make sure the output array has the expected bounds and elements.
    if (.not. associated (rr)) stop 111
    if (lbound (rr, 1) .ne. 1) stop 112
    if (ub0 .eq. lb0) then
      if (ubound (rr, 1) .ne. (ub1 - lb1)/s1 + 1) stop 113
      o1 = 1
      do i1 = lb1, ub1, s1
        if (rr(o1)%x .ne. lb0) stop 114
        if (rr(o1)%y .ne. i1) stop 114
	o1 = o1 + 1
      end do
    else
      if (ubound (rr, 1) .ne. (ub0 - lb0)/s0 + 1) stop 113
      o0 = 1
      do i0 = lb0, ub0, s0
        if (rr(o0)%x .ne. i0) stop 114
        if (rr(o0)%y .ne. lb1) stop 114
	o0 = o0 + 1
      end do
    end if
  end subroutine

end program

