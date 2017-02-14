! { dg-do run }
!
! Contributed by Andre Vehreschild
! Check more elaborate class array addressing.

module m1

  type InnerBaseT
    integer, allocatable :: a(:)
  end type InnerBaseT

  type, extends(InnerBaseT) :: InnerT
    integer :: i
  end type InnerT

  type BaseT
    class(InnerT), allocatable :: arr(:,:)
  contains
    procedure P
  end type BaseT

contains

  subroutine indir(this, mat)
    class(BaseT) :: this
    class(InnerT), intent(inout) :: mat(:,:)

    call this%P(mat)
  end subroutine indir

  subroutine P(this, mat)
    class(BaseT) :: this
    class(InnerT), intent(inout) :: mat(:,:)
    integer :: i,j

    mat%i = 42
    do i= 1, ubound(mat, 1)
      do j= 1, ubound(mat, 2)
        if (.not. allocated(mat(i,j)%a)) then
          allocate(mat(i,j)%a(10), source = 72)
        end if
      end do
    end do
    mat(1,1)%i = 9
    mat(1,1)%a(5) = 1
  end subroutine

end module m1

program test
  use m1

  class(BaseT), allocatable, target :: o
  class(InnerT), pointer :: i_p(:,:)
  class(InnerBaseT), allocatable :: i_a(:,:)
  integer i,j,l

  allocate(o)
  allocate(o%arr(2,2))
  allocate(InnerT::i_a(2,2))
  o%arr%i = 1

  i_p => o%arr
  call o%P(i_p)
  if (any(o%arr%i /= reshape([9,42,42,42],[2,2]))) call abort()
  do l= 1, 10
    do i= 1, 2
      do j= 1,2
        if ((i == 1 .and. j == 1 .and. l == 5 .and. &
             o%arr(i,j)%a(5) /= 1) &
            .or. (.not. (i == 1 .and. j == 1 .and. l == 5) &
              .and. o%arr(i,j)%a(l) /= 72)) call abort()
      end do
    end do
  end do

  select type (i_a)
    type is (InnerT)
      call o%P(i_a)
      do l= 1, 10
        do i= 1, 2
          do j= 1,2
            if ((i == 1 .and. j == 1 .and. l == 5 .and. &
                 i_a(i,j)%a(5) /= 1) &
                .or. (.not. (i == 1 .and. j == 1 .and. l == 5) &
                  .and. i_a(i,j)%a(l) /= 72)) call abort()
          end do
        end do
      end do
  end select

  i_p%i = 4
  call indir(o, i_p)
  if (any(o%arr%i /= reshape([9,42,42,42],[2,2]))) call abort()
end program test

! vim:ts=2:sts=2:cindent:sw=2:tw=80:
