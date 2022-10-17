! { dg-do run }
!
! TS 29113
! 6.4.1  SHAPE
!
! The description of the intrinsic function SHAPE in ISO/IEC 1539-1:2010
! is changed for an assumed-rank array that is associated with an
! assumed-size array; an assumed-size array has no shape, but in this
! case the result has a value equal to 
! [ (SIZE (ARRAY, I, KIND), I=1,RANK (ARRAY)) ] 
! with KIND omitted from SIZE if it was omitted from SHAPE.
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

contains

  subroutine testit (a) bind(c)
    integer :: a(..)
    
    integer :: r
    r = rank(a)

    block
      integer :: s(r)
      s = shape(a)
      do i = 1, r
        if (s(i) .ne. size(a,i)) stop 101
      end do
    end block

  end subroutine

  subroutine test1 (a) bind(c)
    integer :: a(*)

    call testit (a)
  end subroutine

  subroutine test3 (a, l1, u1, l2, u2) bind(c)
    implicit none
    integer :: l1, u1, l2, u2
    integer :: a(l1:u1, l2:u2, *)

    call testit (a)
  end subroutine

end program
