! { dg-do run }
! PR fortran/114827 - issues with class(*) assignment found by valgrind
!
! Contributed by Neil Carlson <neil.n.carlson@gmail.com>

program main
  implicit none
  call run
  call run1
  call run2
contains
  ! Scalar tests
  subroutine run ()
    character(*),        parameter :: c = 'fubarfubarfubarfubarfubarfu'
    character(*,kind=4), parameter :: d = 4_"abcdef"
    complex,             parameter :: z = (1.,2.)
    class(*),          allocatable :: y

    call foo (c, y)
    select type (y)
    type is (character(*))
!      print *, y(5:6)                  ! ICE (-> pr114874)
       if (y /= c) stop 1
    class default
       stop 2
    end select

    call foo (z, y)
    select type (y)
    type is (complex)
       if (y /= z) stop 3
    class default
       stop 4
    end select

    call foo (d, y)
    select type (y)
    type is (character(*,kind=4))
!      print *, y                       ! NAG fails here
       if (y /= d) stop 5
    class default
       stop 6
    end select
  end subroutine 
  !
  subroutine foo (a, b)
    class(*), intent(in)  :: a
    class(*), allocatable :: b
    b = a
  end subroutine

  ! Rank-1 tests
  subroutine run1 ()
    character(*),        parameter :: c(*) = ['fubar','snafu']
    character(*,kind=4), parameter :: d(*) = [4_"abc",4_"def"]
    real,                parameter :: r(*) = [1.,2.,3.]
    class(*),          allocatable :: y(:)

    call foo1 (c, y)
    select type (y)
    type is (character(*))
!      print *, ">",y(2)(1:3),"<  >", c(2)(1:3), "<"
       if (any (y    /= c))        stop 11
       if (y(2)(1:3) /= c(2)(1:3)) stop 12
    class default
       stop 13
    end select

    call foo1 (r, y)
    select type (y)
    type is (real)
       if (any (y /= r)) stop 14
    class default
       stop 15
    end select

    call foo1 (d, y)
    select type (y)
    type is (character(*,kind=4))
!      print *, ">",y(2)(2:3),"<  >", d(2)(2:3), "<"
       if (any (y /= d)) stop 16
    class default
       stop 17
    end select
  end subroutine 
  !
  subroutine foo1 (a, b)
    class(*), intent(in)  :: a(:)
    class(*), allocatable :: b(:)
    b = a
  end subroutine

  ! Rank-2 tests
  subroutine run2 ()
    character(7) :: c(2,3)
    complex      :: z(3,3)
    integer      :: i, j
    class(*), allocatable :: y(:,:)

    c = reshape (['fubar11','snafu21',&
                  'fubar12','snafu22',&
                  'fubar13','snafu23'],shape(c))
    call foo2 (c, y)
    select type (y)
    type is (character(*))
!      print *, y(2,1)
       if (y(2,1) /= c(2,1)) stop 21
       if (any (y /= c))     stop 22
    class default
       stop 23
    end select

    do    j = 1, size (z,2)
       do i = 1, size (z,1)
          z(i,j) = cmplx (i,j)
       end do
    end do
    call foo2 (z, y)
    select type (y)
    type is (complex)
!      print *, y(2,1)
       if (any (y%re /= z%re)) stop 24
       if (any (y%im /= z%im)) stop 25
    class default
       stop 26
    end select
  end subroutine 
  !
  subroutine foo2 (a, b)
    class(*), intent(in)  :: a(:,:)
    class(*), allocatable :: b(:,:)
    b = a
  end subroutine

end program
