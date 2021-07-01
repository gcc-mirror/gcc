! { dg-do run }
! { dg-additional-sources note-5-4-c.c }
!
! TS 29113
! NOTE 5.4
! Assumed rank is an attribute of a Fortran dummy argument. When a C
! function is invoked with an actual argument that corresponds to an 
! assumed-rank dummy argument in a Fortran interface for that C function,
! the corresponding formal parameter is the address of a descriptor of 
! type CFI_cdesc_t (8.7). The rank member of the descriptor provides the
! rank of the actual argument. The C function should therefore be able
! to handle any rank. On each invocation, the rank is available to it.

program test

  interface
    function test_rank (a) bind (c, name="test_rank")
      integer :: test_rank
      integer :: a(..)
    end function
  end interface

  integer ::  scalar, array_1d(10), array_2d(3, 3)

  call testit (scalar, array_1d, array_2d)

contains

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

    ! allocatables
    allocate (c0)
    allocate (c1 (10))
    allocate (c2 (3, 3))
    if (test_rank (c0) .ne. 0) stop 300
    if (test_rank (c1) .ne. 1) stop 301
    if (test_rank (c2) .ne. 2) stop 302

    ! pointers
    d0 => b0
    d1 => b1
    d2 => b2
    if (test_rank (d0) .ne. 0) stop 400
    if (test_rank (d1) .ne. 1) stop 401
    if (test_rank (d2) .ne. 2) stop 402

  end subroutine
end program
