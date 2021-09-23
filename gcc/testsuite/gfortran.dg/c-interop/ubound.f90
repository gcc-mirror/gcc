! { dg-do run }
!
! TS 29113
! 6.4.3  UBOUND
!
! The description of the intrinsic function UBOUND in ISO/IEC
! 1539-1:2010 is changed for an assumed-rank object that is associated
! with an assumed-size array; the result of UBOUND (ARRAY, RANK(ARRAY),
! KIND) has a value equal to LBOUND (ARRAY, RANK (ARRAY), KIND) −2 with
! KIND omitted from LBOUND if it was omitted from UBOUND.
!
! NOTE 6.2  
! If LBOUND or UBOUND is invoked for an assumed-rank object that is
! associated with a scalar and DIM is absent, the result is a zero-sized
! array. LBOUND or UBOUND cannot be invoked for an assumed-rank object
! that is associated with a scalar if DIM is present because the rank of
! a scalar is zero and DIM must be ≥ 1.
!
! The idea here is that the main program passes some arrays to a test
! subroutine with an assumed-size dummy, which in turn passes that to a
! subroutine with an assumed-rank dummy.

program test 

  ! Define some arrays for testing.
  integer, target :: x1(5)
  integer :: y1(0:9)
  integer, pointer :: p1(:)
  integer, allocatable :: a1(:)
  integer, target :: x3(2,3,4)
  integer :: y3(0:1,-3:-1,4)
  integer, pointer :: p3(:,:,:)
  integer, allocatable :: a3(:,:,:)
  integer :: x

  ! Test the 1-dimensional arrays.
  call test1 (x1)
  call testit2(x1, shape(x1))
  call test1 (y1)
  call testit2(y1, shape(y1))
  p1 => x1
  call testit2(p1, shape(p1))
  call testit2p(p1, lbound(p1), shape(p1))
  call test1 (p1)
  p1(77:) => x1
  call testit2p(p1, [77], shape(p1))
  allocate (a1(5))
  call testit2(a1, shape(a1))
  call testit2a(a1, lbound(a1), shape(a1))
  call test1 (a1)
  deallocate(a1)
  allocate (a1(-38:5))
  call test1 (a1)
  call testit2(a1, shape(a1))
  call testit2a(a1, [-38], shape(a1))

  ! Test the multi-dimensional arrays.
  call test3 (x3, 1, 2, 1, 3)
  call test3 (y3, 0, 1, -3, -1)
  p3 => x3
  call test3 (p3, 1, 2, 1, 3)
  allocate (a3(2,3,4))
  call test3 (a3, 1, 2, 1, 3)

  ! Test some scalars.
  call test0 (x)
  call test0 (-1)
  call test0 (x1(1))

contains

  subroutine testit (a)
    integer :: a(..)
    integer :: r
    r = rank(a)
    if (any (lbound (a) .ne. 1)) stop 101
    if (ubound (a, r) .ne. -1) stop 102
  end subroutine

  subroutine testit2(a, shape)
    integer :: a(..)
    integer :: shape(:)
    if (rank(a) /= size(shape)) stop 111
    if (any (lbound(a) /= 1)) stop 112
    if (any (ubound(a) /= shape)) stop 113
  end subroutine

  subroutine testit2a(a,lbound2,  shape2)
    integer, allocatable :: a(..)
    integer :: lbound2(:), shape2(:)
    if (rank(a) /= size(shape2)) stop 121
    if (any (lbound(a) /= lbound2)) stop 122
    if (any (ubound(a) /= lbound2 + shape2 - 1)) stop 123
    if (any (shape(a) /= shape2)) stop 124
    if (sum (shape(a)) /= size(a)) stop 125
  end subroutine

  subroutine testit2p(a, lbound2, shape2)
    integer, pointer :: a(..)
    integer :: lbound2(:), shape2(:)
    if (rank(a) /= size(shape2)) stop 131
    if (any (lbound(a) /= lbound2)) stop 132
    if (any (ubound(a) /= lbound2 + shape2 - 1)) stop 133
    if (any (shape(a) /= shape2)) stop 134
    if (sum (shape(a)) /= size(a)) stop 135
  end subroutine 

  subroutine test0 (a)
    integer :: a(..)
    if (rank (a) .ne. 0) stop 141
    if (size (lbound (a)) .ne. 0) stop 142
    if (size (ubound (a)) .ne. 0) stop 143
  end subroutine

  subroutine test1 (a)
    integer :: a(*)

    call testit (a)
  end subroutine

  subroutine test3 (a, l1, u1, l2, u2)
    implicit none
    integer :: l1, u1, l2, u2
    integer :: a(l1:u1, l2:u2, *)

    call testit (a)
  end subroutine

end program
