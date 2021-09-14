! PR 101310
! { dg-do run }
! { dg-additional-sources "section-2-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program tests basic use of the CFI_section C library function on
! a 2-dimensional pointer array.

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
    subroutine ctest (p, lb0, lb1, ub0, ub1, s0, s1, r) bind (c)
      use iso_c_binding
      use mm
      type(m), pointer :: p(:,:)
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

  call test (aa, 0, 0, 3, 2, 9, 14, 2, 3)       ! zero lower bound
  call test (aa, 1, 1, 4, 3, 10, 15, 2, 3)      ! lower bound 1
  call test (aa, 6, 11, 9, 13, 15, 25, 2, 3)    ! other lower bound
  call test (aa, 1, 1, 10, 15, 4, 3, -2, -3)    ! negative step
  stop

contains

  ! Test function for pointer array AA.
  ! The bounds of the array are adjusted so it is based at (LO0,LO1).
  ! LB, UB, and S describe the section of the adjusted array to take.
  subroutine test (aa, lo0, lo1, lb0, lb1, ub0, ub1, s0, s1)
    use mm
    type(m), target :: aa(1:10, 1:20)
    integer :: lo0, lo1, lb0, lb1, ub0, ub1, s0, s1

    type(m), pointer :: pp(:,:), rr(:,:)
    integer :: i0, i1, o0, o1
    integer :: hi0, hi1
    hi0 = lo0 + 10 - 1
    hi1 = lo1 + 20 - 1

    pp(lo0:,lo1:) => aa
    if (lbound (pp, 1) .ne. lo0) stop 121
    if (lbound (pp, 2) .ne. lo1) stop 121
    if (ubound (pp, 1) .ne. hi0) stop 122
    if (ubound (pp, 2) .ne. hi1) stop 122
    nullify (rr)
    call ctest (pp, lb0, lb1, ub0, ub1, s0, s1, rr)

    ! Make sure the input pointer array has not been modified.
    if (lbound (pp, 1) .ne. lo0) stop 131
    if (ubound (pp, 1) .ne. hi0) stop 132
    if (lbound (pp, 2) .ne. lo1) stop 133
    if (ubound (pp, 2) .ne. hi1) stop 134
    do i1 = lo1, hi1
      do i0 = lo0, hi0
        if (pp(i0,i1)%x .ne. i0 - lo0 + 1) stop 135
        if (pp(i0,i1)%y .ne. i1 - lo1 + 1) stop 136
      end do
    end do

    ! Make sure the output array has the expected bounds and elements.
    if (.not. associated (rr)) stop 141
    if (lbound (rr, 1) .ne. 1) stop 142
    if (lbound (rr, 2) .ne. 1) stop 142
    if (ubound (rr, 1) .ne. (ub0 - lb0)/s0 + 1) stop 143
    if (ubound (rr, 2) .ne. (ub1 - lb1)/s1 + 1) stop 143
    o1 = 1
    do i1 = lb1, ub1, s1
      o0 = 1
      do i0 = lb0, ub0, s0
        if (rr(o0,o1)%x .ne. i0 - lo0 + 1) stop 144
        if (rr(o0,o1)%y .ne. i1 - lo1 + 1) stop 144
        o0 = o0 + 1
      end do
      o1 = o1 + 1
    end do
  end subroutine

end program

