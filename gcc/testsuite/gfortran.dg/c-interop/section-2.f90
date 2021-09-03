! { dg-do run }
! { dg-additional-sources "section-2-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program tests basic use of the CFI_section C library function on
! a 2-dimensional non-pointer array.

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
      type(m), pointer, intent(out) :: r(:,:)
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

  call test (aa, 4, 3, 10, 15, 2, 3)       ! basic test
  call test (aa, 10, 15, 4, 3, -2, -3)     ! negative step
  stop

contains

  ! Test function for non-pointer array AA.
  ! LB, UB, and S describe the section to take.
  subroutine test (aa, lb0, lb1, ub0, ub1, s0, s1)
    use mm
    type(m) :: aa(10,20)
    integer :: lb0, lb1, ub0, ub1, s0, s1

    type(m), pointer :: rr(:,:)
    integer :: i0, i1, o0, o1
    integer, parameter :: hi0 = 10
    integer, parameter :: hi1 = 20

    ! Make sure the original array is OK.
    do i1 = 1, hi1
      do i0 = 1, hi0
        if (aa(i0,i1)%x .ne. i0) stop 101
        if (aa(i0,i1)%y .ne. i1) stop 101
      end do
    end do

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
    if (lbound (rr, 2) .ne. 1) stop 112
    if (ubound (rr, 1) .ne. (ub0 - lb0)/s0 + 1) stop 113
    if (ubound (rr, 2) .ne. (ub1 - lb1)/s1 + 1) stop 113
    o1 = 1
    do i1 = lb1, ub1, s1
      o0 = 1
      do i0 = lb0, ub0, s0
        ! print 999, o0, o1, rr(o0,o1)%x, rr(o0,01)%y
	! 999 format ('rr(', i3, ',', i3, ') = (', i3, ',', i3, ')')
        if (rr(o0,o1)%x .ne. i0) stop 114
        if (rr(o0,o1)%y .ne. i1) stop 114
        o0 = o0 + 1
      end do
      o1 = o1 + 1
    end do
  end subroutine

end program

