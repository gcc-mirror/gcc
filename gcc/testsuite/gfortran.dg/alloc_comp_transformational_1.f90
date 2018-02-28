! { dg-do run }
! Tests the fix for PR41478, in which double frees would occur because
! transformational intrinsics did not copy the allocatable components
! so that they were (sometimes) freed twice on exit.  In addition,
! The original allocatable components of a1 were not freed, so that
! memory leakage occurred.
!
! Contributed by Juergen Reuter <reuter@physik.uni-freiburg.de>
!
  type :: container_t
    integer, dimension(:), allocatable :: entry
    integer index
  end type container_t
  call foo
  call bar
contains
!
! This is the reported problem.
!
  subroutine foo
    type(container_t), dimension(4) :: a1, a2, a3
    integer :: i
    do i = 1, 4
      allocate (a1(i)%entry (2), a2(i)%entry (2), a3(i)%entry (2))
      a1(i)%entry = [1,2]
      a2(i)%entry = [3,4]
      a3(i)%entry = [4,5]
      a1(i)%index = i
      a2(i)%index = i
      a3(i)%index = i
    end do
    a1(1:2) = pack (a2, [.true., .false., .true., .false.])
    do i = 1, 4
      if (.not.allocated (a1(i)%entry)) STOP 1
      if (i .gt. 2) then
        if (any (a1(i)%entry .ne. [1,2])) STOP 2
      else
        if (any (a1(i)%entry .ne. [3,4])) STOP 3
      end if
    end do
!
! Now check unpack
!
    a1 = unpack (a1, [.true., .true., .false., .false.], a3)
    if (any (a1%index .ne. [1,3,3,4])) STOP 4
    do i = 1, 4
      if (.not.allocated (a1(i)%entry)) STOP 5
      if (i .gt. 2) then
        if (any (a1(i)%entry .ne. [4,5])) STOP 6
      else
        if (any (a1(i)%entry .ne. [3,4])) STOP 7
      end if
    end do
  end subroutine
!
! Other all transformational intrinsics display it. Having done
! PACK and UNPACK, just use TRANSPOSE as a demonstrator.
!
  subroutine bar
    type(container_t), dimension(2,2) :: a1, a2
    integer :: i, j
    do i = 1, 2
      do j = 1, 2
        allocate (a1(i, j)%entry (2), a2(i, j)%entry (2))
        a1(i, j)%entry = [i,j]
        a2(i, j)%entry = [i,j]
        a1(i,j)%index = j + (i - 1)*2
        a2(i,j)%index = j + (i - 1)*2
      end do
    end do
    a1 = transpose (a2)
    do i = 1, 2
      do j = 1, 2
        if (a1(i,j)%index .ne. i + (j - 1)*2) STOP 8
        if (any (a1(i,j)%entry .ne. [j,i])) STOP 9
      end do
    end do
  end subroutine
end

