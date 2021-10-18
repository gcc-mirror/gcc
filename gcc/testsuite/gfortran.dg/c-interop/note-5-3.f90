! { dg-do run }
!
! TS 29113
! NOTE 5.3
! The intrinsic inquiry function RANK can be used to inquire about the 
! rank of a data object. The rank of an assumed-rank object is zero if 
! the rank of the corresponding actual argument is zero.

program test

  integer ::  scalar, array_1d(10), array_2d(3, 3)

  call testit (scalar, array_1d, array_2d)

contains

  function test_rank (a)
    integer :: test_rank
    integer :: a(..)

    test_rank = rank (a)
  end function

  subroutine testit (a0, a1, a2)
    integer :: a0(..), a1(..), a2(..)

    integer, target :: b0, b1(10), b2(3, 3)
    integer, allocatable :: c0, c1(:), c2(:,:)
    integer, pointer :: d0, d1(:), d2(:,:)

    ! array descriptor passed from caller through testit to test_rank
    if (test_rank (a0) .ne. 0) stop 100
    if (test_rank (a1) .ne. 1) stop 101
    if (test_rank (a2) .ne. 2) stop 102

    ! array descriptor created locally here, fixed size
    if (test_rank (b0) .ne. 0) stop 200
    if (test_rank (b1) .ne. 1) stop 201
    if (test_rank (b2) .ne. 2) stop 202

    ! allocatable arrays don't actually have to be allocated.
    if (test_rank (c0) .ne. 0) stop 300
    if (test_rank (c1) .ne. 1) stop 301
    if (test_rank (c2) .ne. 2) stop 302

    ! pointer arrays do need to point at something.
    d0 => b0
    d1 => b1
    d2 => b2
    if (test_rank (d0) .ne. 0) stop 400
    if (test_rank (d1) .ne. 1) stop 401
    if (test_rank (d2) .ne. 2) stop 402

  end subroutine
end program
