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
!
! This is the polymorphic version of shape.f90.

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

  subroutine testit (a)
    use m
    class(t) :: a(..)
    
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

  subroutine test1 (a)
    use m
    class(t) :: a(*)

    call testit (a)
  end subroutine

  subroutine test3 (a, l1, u1, l2, u2)
    use m
    integer :: l1, u1, l2, u2
    class(t) :: a(l1:u1, l2:u2, *)

    call testit (a)
  end subroutine

end program
