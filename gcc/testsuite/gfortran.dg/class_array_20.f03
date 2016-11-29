! { dg-do run }
!
! Test contributed by Thomas L. Clune via pr60322
!                  and Antony Lewis via pr64692

program class_array_20
  implicit none

  type Foo
  end type

  type(foo), dimension(2:3) :: arg
  integer :: oneDarr(2)
  integer :: twoDarr(2,3)
  integer :: x, y
  double precision :: P(2, 2)

  ! Checking for PR/60322
  call copyFromClassArray([Foo(), Foo()])
  call copyFromClassArray(arg)
  call copyFromClassArray(arg(:))

  x= 3
  y= 4
  oneDarr = [x, y]
  call W([x, y])
  call W(oneDarr)
  call W([3, 4])

  twoDarr = reshape([3, 4, 5, 5, 6, 7], [2, 3])
  call WtwoD(twoDarr)
  call WtwoD(reshape([3, 4, 5, 5, 6, 7], [2, 3]))

  ! Checking for PR/64692
  P(1:2, 1) = [1.d0, 2.d0]
  P(1:2, 2) = [3.d0, 4.d0]
  call AddArray(P(1:2, 2))

contains

  subroutine copyFromClassArray(classarray)
    class (Foo), intent(in) :: classarray(:)

    if (lbound(classarray, 1) .ne. 1) call abort()
    if (ubound(classarray, 1) .ne. 2) call abort()
    if (size(classarray) .ne. 2) call abort()
  end subroutine

  subroutine AddArray(P)
    class(*), target, intent(in) :: P(:)
    class(*), pointer :: Pt(:)

    allocate(Pt(1:size(P)), source= P)

    select type (P)
      type is (double precision)
        if (abs(P(1)-3.d0) .gt. 1.d-8) call abort()
        if (abs(P(2)-4.d0) .gt. 1.d-8) call abort()
      class default
        call abort()
    end select

    select type (Pt)
      type is (double precision)
        if (abs(Pt(1)-3.d0) .gt. 1.d-8) call abort()
        if (abs(Pt(2)-4.d0) .gt. 1.d-8) call abort()
      class default
        call abort()
    end select
  end subroutine

  subroutine W(ar)
    class(*), intent(in) :: ar(:)

    if (lbound(ar, 1) /= 1) call abort()
    select type (ar)
      type is (integer)
        ! The indeces 1:2 are essential here, or else one would not
        ! note, that the array internally starts at 0, although the
        ! check for the lbound above went fine.
        if (any (ar(1:2) .ne. [3, 4])) call abort()
      class default
        call abort()
    end select
  end subroutine

  subroutine WtwoD(ar)
    class(*), intent(in) :: ar(:,:)

    if (any (lbound(ar) /= [1, 1])) call abort()
    select type (ar)
      type is (integer)
        if (any (reshape(ar(1:2,1:3), [6]) .ne. [3, 4, 5, 5, 6, 7])) &
        call abort()
      class default
        call abort()
    end select
  end subroutine
end program class_array_20

