! Reported as pr94070.
! { dg-do run }
!
! TS 29113
! 6.4.2 SIZE
!
! The description of the intrinsic function SIZE in ISO/IEC 1539-1:2010
! is changed in the following cases:
!
! (1) for an assumed-rank object that is associated with an assumed-size
! array, the result has the value −1 if DIM is present and equal to the
! rank of ARRAY, and a negative value that is equal to 
! PRODUCT ( [ (SIZE (ARRAY, I, KIND), I=1, RANK (ARRAY)) ] ) 
! if DIM is not present;
!
! (2) for an assumed-rank object that is associated with a scalar, the
! result has the value 1.
!
! The idea here is that the main program passes some arrays to a test
! subroutine with an assumed-size dummy, which in turn passes that to a
! subroutine with an assumed-rank dummy.
!
! This is the polymorphic version of size.f90.

module m
  type :: t
    integer :: id
    real :: xyz(3)
  end type
end module

program test
  use m

  ! Define some arrays for testing.
  type(t), target :: x1(5)
  type(t) :: y1(0:9)
  class(t), pointer :: p1(:)
  class(t), allocatable :: a1(:)
  type(t), target :: x3(2,3,4)
  type(t) :: y3(0:1,-3:-1,4)
  class(t), pointer :: p3(:,:,:)
  type(t), allocatable :: a3(:,:,:)
  type(t) :: x

  ! Test the 1-dimensional arrays.
  call test1 (x1)
  call test1 (y1)
  p1 => x1
  call test1 (p1)
  allocate (a1(5))
  call test1 (a1)

  ! Test the multi-dimensional arrays.
  call test3 (x3, 1, 2, 1, 3)
  call test3 (y3, 0, 1, -3, -1)
  p3 => x3
  call test3 (p3, 1, 2, 1, 3)
  allocate (a3(2,3,4))
  call test3 (a3, 1, 2, 1, 3)

  ! Test scalars.
  call test0 (x)
  call test0 (x1(1))

contains

  subroutine testit (a, r, sizes)
    use m
    class(t) :: a(..)
    integer :: r
    integer :: sizes(r)
    
    integer :: totalsize, thissize
    totalsize = 1

    if (r .ne. rank(a))  stop 101

    do i = 1, r
      thissize = size (a, i)
      print *, 'got size ', thissize, ' expected ', sizes(i)
      if (thissize .ne. sizes(i)) stop 102
      totalsize = totalsize * thissize
    end do

    if (size(a) .ne. totalsize) stop 103
  end subroutine

  subroutine test0 (a)
    use m
    class(t) :: a(..)

    if (size (a) .ne. 1) stop 103
  end subroutine

  subroutine test1 (a)
    use m
    class(t) :: a(*)

    integer :: sizes(1)
    sizes(1) = -1
    call testit (a, 1, sizes)
  end subroutine

  subroutine test3 (a, l1, u1, l2, u2)
    use m
    integer :: l1, u1, l2, u2
    class(t) :: a(l1:u1, l2:u2, *)

    integer :: sizes(3)
    sizes(1) = u1 - l1 + 1
    sizes(2) = u2 - l2 + 1
    sizes(3) = -1

    call testit (a, 3, sizes)
  end subroutine

end program
