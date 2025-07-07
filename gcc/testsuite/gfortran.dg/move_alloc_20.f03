! { dg-do run }
!
! Check the presence of the pre and post code of the FROM and TO arguments
! of the MOVE_ALLOC intrinsic subroutine.

module m
  implicit none
  type :: t
    integer, allocatable :: a(:)
  end type
end module 

module pre
  use m
  implicit none
  private
  public :: check_pre

contains

  subroutine check_pre
    integer, parameter :: n = 5
    type(t) :: x(n)
    integer, allocatable :: tmp(:)
    integer :: array(4) = [ -1, 0, 1, 2 ]
    integer :: i

    if (allocated(tmp)) error stop 1

    tmp = [17]

    if (.not. allocated(tmp)) error stop 11
    if (any(shape(tmp) /= [1])) error stop 12
    if (any(tmp /= [17])) error stop 13
    do i=1,n
      if (allocated(x(i)%a)) error stop 14
    end do

    ! Check that the index of X is properly computed for the evaluation of TO.
    call move_alloc(tmp, x(sum(array))%a)

    do i=1,n
      if (i == 2) cycle
      if (allocated(x(i)%a)) error stop 21
    end do
    if (.not. allocated(x(2)%a)) error stop 22
    if (any(shape(x(2)%a) /= [1])) error stop 23
    if (any(x(2)%a /= [17])) error stop 24
    if (allocated(tmp)) error stop 25

    ! Check that the index of X is properly computed for the evaluation of FROM.
    call move_alloc(x(sum(array))%a, tmp)

    if (.not. allocated(tmp)) error stop 31
    if (any(shape(tmp) /= [1])) error stop 32
    if (any(tmp /= [17])) error stop 33
    do i=1,n
      if (allocated(x(i)%a)) error stop 34
    end do
  end subroutine

end module

module post
  use m
  implicit none
  private
  public :: check_post
  integer, parameter :: n = 5
  type(t), target :: x(n)
  type :: u
    integer :: a
  contains
    final :: finalize
  end type
  integer :: finalization_count = 0

contains

  function idx(arg)
    type(u) :: arg
    integer :: idx
    idx = mod(arg%a, n)
  end function

  subroutine check_post
    type(u) :: y
    integer, allocatable :: tmp(:)
    integer, target :: array(4) = [ -1, 0, 1, 2 ]
    integer :: i

    y%a = 12

    if (allocated(tmp)) error stop 1

    tmp = [37]

    if (.not. allocated(tmp)) error stop 11
    if (any(shape(tmp) /= [1])) error stop 12
    if (any(tmp /= [37])) error stop 13
    if (finalization_count /= 0) error stop 14
    do i=1,n
      if (allocated(x(i)%a)) error stop 15
    end do

    ! Check that the cleanup code for the evaluation of TO is properly
    ! executed after MOVE_ALLOC: the result of GET_U should be finalized.
    call move_alloc(tmp, x(idx(get_u(y)))%a)

    do i=1,n
      if (i == 2) cycle
      if (allocated(x(i)%a)) error stop 21
    end do
    if (.not. allocated(x(2)%a)) error stop 22
    if (any(shape(x(2)%a) /= [1])) error stop 23
    if (any(x(2)%a /= [37])) error stop 24
    if (allocated(tmp)) error stop 25
    if (finalization_count /= 1) error stop 26

    ! Check that the cleanup code for the evaluation of FROM is properly
    ! executed after MOVE_ALLOC: the result of GET_U should be finalized.
    call move_alloc(x(idx(get_u(y)))%a, tmp)

    if (.not. allocated(tmp)) error stop 31
    if (any(shape(tmp) /= [1])) error stop 32
    if (any(tmp /= [37])) error stop 33
    if (finalization_count /= 2) error stop 34
    do i=1,n
      if (allocated(x(i)%a)) error stop 35
    end do
  end subroutine

  function get_u(arg)
    type(u) :: arg, get_u
    get_u = arg
  end function get_u

  subroutine finalize(obj)
    type(u) :: obj
    finalization_count = finalization_count + 1
  end subroutine

end module

program p
  use pre
  use post
  implicit none
  call check_pre
  call check_post
end program
