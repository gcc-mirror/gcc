! PR 101310
! { dg-do run }
! { dg-additional-sources "section-3-c.c dump-descriptors.c" }
! { dg-additional-options "-g" }
!
! This program tests basic use of the CFI_section C library function to
! take a slice of a 2-dimensional pointer array.

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

  ! Zero lower bound
  call test (aa, 0, 0, 2, 0, 2, 19, 0, 1)        ! full slice 0
  call test (aa, 0, 0, 0, 7, 9, 7, 1, 0)         ! full slice 1
  call test (aa, 0, 0, 2, 4, 2, 13, 0, 3)        ! partial slice 0
  call test (aa, 0, 0, 1, 7, 9, 7, 2, 0)         ! partial slice 1
  call test (aa, 0, 0, 2, 13, 2, 4, 0, -3)       ! backwards slice 0
  call test (aa, 0, 0, 9, 7, 1, 7, -2, 0)        ! backwards slice 1

  ! Lower bound 1
  call test (aa, 1, 1, 3, 1, 3, 20, 0, 1)        ! full slice 0
  call test (aa, 1, 1, 1, 8, 10, 8, 1, 0)        ! full slice 1
  call test (aa, 1, 1, 3, 5, 3, 14, 0, 3)        ! partial slice 0
  call test (aa, 1, 1, 2, 8, 10, 8, 2, 0)        ! partial slice 1
  call test (aa, 1, 1, 3, 14, 3, 5, 0, -3)       ! backwards slice 0
  call test (aa, 1, 1, 10, 8, 2, 8, -2, 0)       ! backwards slice 1

  ! Some other lower bound
  call test (aa, 2, 3, 4, 3, 4, 22, 0, 1)        ! full slice 0
  call test (aa, 2, 3, 2, 10, 11, 10, 1, 0)      ! full slice 1
  call test (aa, 2, 3, 4, 7, 4, 16, 0, 3)        ! partial slice 0
  call test (aa, 2, 3, 3, 10, 11, 10, 2, 0)      ! partial slice 1
  call test (aa, 2, 3, 4, 16, 4, 7, 0, -3)       ! backwards slice 0
  call test (aa, 2, 3, 11, 10, 3, 10, -2, 0)     ! backwards slice 1

contains

  subroutine test (aa, lo0, lo1, lb0, lb1, ub0, ub1, s0, s1)
    use mm
    type(m), target :: aa(10,20)
    integer :: lo0, lo1, lb0, lb1, ub0, ub1, s0, s1

    type(m), pointer :: pp(:,:), rr(:)
    integer :: i0, i1, o0, o1

    integer :: hi0, hi1
    hi0 = lo0 + 10 - 1
    hi1 = lo1 + 20 - 1

    ! Check the bounds actually specify a "slice" rather than a subarray.
    if (lb0 .ne. ub0 .and. lb1 .ne. ub1)  stop 100

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
    if (.not. associated (rr)) stop 111
    if (lbound (rr, 1) .ne. 1) stop 112
    if (ub0 .eq. lb0) then
      if (ubound (rr, 1) .ne. (ub1 - lb1)/s1 + 1) stop 113
      o1 = 1
      do i1 = lb1, ub1, s1
        if (rr(o1)%x .ne. lb0 - lo0 + 1) stop 114
        if (rr(o1)%y .ne. i1 - lo1 + 1) stop 114
	o1 = o1 + 1
      end do
    else
      if (ubound (rr, 1) .ne. (ub0 - lb0)/s0 + 1) stop 113
      o0 = 1
      do i0 = lb0, ub0, s0
        if (rr(o0)%x .ne. i0 - lo0 + 1) stop 114
        if (rr(o0)%y .ne. lb1 - lo1 + 1) stop 114
	o0 = o0 + 1
      end do
    end if
  end subroutine

end program

