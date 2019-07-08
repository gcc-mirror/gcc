! { dg-do run }
!
! Test the fix for PR91077 - both the original test and that in comment #4 of the PR.
!
! Contribute by Ygal Klein  <ygalklein@gmail.com>
!
program test
  implicit none
  call original
  call comment_4
contains
  subroutine original
    integer, parameter :: length = 9
    real(8), dimension(2) :: a, b
    integer :: i
    type point
       real(8) :: x
    end type point

    type stored
       type(point), dimension(:), allocatable :: np
    end type stored
    type(stored), dimension(:), pointer :: std =>null()
    allocate(std(1))
    allocate(std(1)%np(length))
    std(1)%np(1)%x = 0.3d0
    std(1)%np(2)%x = 0.3555d0
    std(1)%np(3)%x = 0.26782d0
    std(1)%np(4)%x = 0d0
    std(1)%np(5)%x = 1.555d0
    std(1)%np(6)%x = 7.3d0
    std(1)%np(7)%x = 7.8d0
    std(1)%np(8)%x = 6.3d0
    std(1)%np(9)%x = 5.5d0
!    do i = 1, 2
!       write(*, "('std(1)%np(',i1,')%x = ',1e22.14)") i, std(1)%np(i)%x
!    end do
!    do i = 1, 2
!       write(*, "('std(1)%np(1:',i1,') = ',9e22.14)") i, std(1)%np(1:i)%x
!    end do
    a = std(1)%np(1:2)%x
    b = [std(1)%np(1)%x, std(1)%np(2)%x]
!    print *,a
!    print *,b
    if (allocated (std(1)%np)) deallocate (std(1)%np)
    if (associated (std)) deallocate (std)
    if (norm2(a - b) .gt. 1d-3) stop 1
  end subroutine

  subroutine comment_4
    integer, parameter :: length = 2
    real(8), dimension(length) :: a, b
    integer :: i

    type point
       real(8) :: x
    end type point

    type points
       type(point), dimension(:), pointer :: np=>null()
    end type points

    type stored
       integer :: l
       type(points), pointer :: nfpoint=>null()
    end type stored

    type(stored), dimension(:), pointer :: std=>null()


    allocate(std(1))
    allocate(std(1)%nfpoint)
    allocate(std(1)%nfpoint%np(length))
    std(1)%nfpoint%np(1)%x = 0.3d0
    std(1)%nfpoint%np(2)%x = 0.3555d0

!    do i = 1, length
!       write(*, "('std(1)%nfpoint%np(',i1,')%x = ',1e22.14)") i, std(1)%nfpoint%np(i)%x
!    end do
!    do i = 1, length
!       write(*, "('std(1)%nfpoint%np(1:',i1,')%x = ',2e22.14)") i, std(1)%nfpoint%np(1:i)%x
!    end do
    a = std(1)%nfpoint%np(1:2)%x
    b = [std(1)%nfpoint%np(1)%x, std(1)%nfpoint%np(2)%x]
    if (associated (std(1)%nfpoint%np)) deallocate (std(1)%nfpoint%np)
    if (associated (std(1)%nfpoint)) deallocate (std(1)%nfpoint)
    if (associated (std)) deallocate (std)
    if (norm2(a - b) .gt. 1d-3) stop 2
    end subroutine
end program test
