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

module m

  type :: base
    integer :: a, b
  end type

  type, extends (base) :: derived
    integer :: c
  end type
end module

program test 
  use m

  ! Define some arrays for testing.
  type(derived), target :: x1(5)
  type(derived) :: y1(0:9)
  type(derived), pointer :: p1(:)
  type(derived), allocatable :: a1(:)
  type(derived), target :: x3(2,3,4)
  type(derived) :: y3(0:1,-3:-1,4)
  type(derived), pointer :: p3(:,:,:)
  type(derived), allocatable :: a3(:,:,:)
  type(derived) :: x

  ! Test the 1-dimensional arrays.
  if (rank (x1) .ne. 1) stop 201
  call testit (x1, 1)
  if (rank (y1) .ne. 1) stop 202
  call testit (y1, 1)
  if (rank (p1) .ne. 1) stop 203
  p1 => x1
  call testit (p1, 1)
  if (rank (p1) .ne. 1) stop 204
  if (rank (a1) .ne. 1) stop 205
  allocate (a1(5))
  if (rank (a1) .ne. 1) stop 206
  call testit (a1, 1)

  ! Test the multi-dimensional arrays.
  if (rank (x3) .ne. 3) stop 207
  call testit (x3, 3)
  if (rank (y3) .ne. 3) stop 208
  if (rank (p3) .ne. 3) stop 209
  p3 => x3
  call testit (p3, 3)
  if (rank (p3) .ne. 3) stop 210
  if (rank (a3) .ne. 3) stop 211
  allocate (a3(2,3,4))
  call testit (a3, 3)
  if (rank (a3) .ne. 3) stop 212

  ! Test scalars.
  if (rank (x) .ne. 0) stop 213
  call testit (x, 0)
  call test0 (x)
  if (rank (x1(1)) .ne. 0) stop 215
  call test0 (x1(1))

contains

  subroutine testit (a, r)
    use m
    class(base) :: a(..)
    integer :: r

    if (r .ne. rank(a))  stop 101
  end subroutine

  subroutine test0 (a)
    use m
    class(base) :: a(..)
    if (rank (a) .ne. 0) stop 103
    call testit (a, 0)
  end subroutine

end program
