! { dg-do run }
!
! TS 29113
! 7.2 RANK (A)
! Description. Rank of a data object.
! Class. Inquiry function.
! Argument.
! A shall be a scalar or array of any type.
! Result Characteristics. Default integer scalar.
! Result Value. The result is the rank of A.

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
  if (rank (x1) .ne. 1) stop 201
  call testit (x1, 1)
  call test1 (x1)
  if (rank (y1) .ne. 1) stop 202
  call testit (y1, 1)
  call test1 (y1)
  if (rank (p1) .ne. 1) stop 203
  p1 => x1
  call testit (p1, 1)
  if (rank (p1) .ne. 1) stop 204
  call test1 (p1)
  if (rank (a1) .ne. 1) stop 205
  allocate (a1(5))
  if (rank (a1) .ne. 1) stop 206
  call testit (a1, 1)
  call test1 (a1)

  ! Test the multi-dimensional arrays.
  if (rank (x3) .ne. 3) stop 207
  call testit (x3, 3)
  call test1 (x3)
  call test3 (x3, 1, 2, 1, 3)
  if (rank (y3) .ne. 3) stop 208
  call test3 (y3, 0, 1, -3, -1)
  if (rank (p3) .ne. 3) stop 209
  p3 => x3
  call testit (p3, 3)
  call test1 (p3)
  if (rank (p3) .ne. 3) stop 210
  call test3 (p3, 1, 2, 1, 3)
  if (rank (a3) .ne. 3) stop 211
  allocate (a3(2,3,4))
  call testit (a3, 3)
  call test1 (a3)
  if (rank (a3) .ne. 3) stop 212
  call test3 (a3, 1, 2, 1, 3)

  ! Test scalars.
  if (rank (x) .ne. 0) stop 213
  call testit (x, 0)
  call test0 (x)
  if (rank (-1) .ne. 0) stop 214
  call test0 (-1)
  if (rank (x1(1)) .ne. 0) stop 215
  call test0 (x1(1))

contains

  subroutine testit (a, r)
    integer :: a(..)
    integer :: r

    if (r .ne. rank(a))  stop 101
  end subroutine

  subroutine test0 (a)
    integer :: a(..)
    if (rank (a) .ne. 0) stop 103
    call testit (a, 0)
  end subroutine

  subroutine test1 (a)
    integer :: a(*)
    call testit (a, 1)
  end subroutine

  subroutine test3 (a, l1, u1, l2, u2)
    implicit none
    integer :: l1, u1, l2, u2
    integer :: a(l1:u1, l2:u2, *)
    call testit (a, 3)
  end subroutine

end program
