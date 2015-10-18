! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! Fix for PR......
!
! The 'to' components of 'mytemp' would remain allocated after the call to
! MOVE_ALLOC, resulting in memory leaks.
!
! Contributed by Alberto Luaces.
!
! See https://groups.google.com/forum/#!topic/comp.lang.fortran/k3bkKUbOpFU
!
module alloctest
  type myallocatable
     integer, allocatable:: i(:)
  end type myallocatable

contains
  subroutine f(num, array)
    implicit none
    integer, intent(in) :: num
    integer :: i
    type(myallocatable):: array(:)

    do i = 1, num
       allocate(array(i)%i(5), source = [1,2,3,4,5])
    end do

  end subroutine f
end module alloctest

program name
  use alloctest
  implicit none
  type(myallocatable), allocatable:: myarray(:), mytemp(:)
  integer, parameter:: OLDSIZE = 7, NEWSIZE = 20
  logical :: flag

  allocate(myarray(OLDSIZE))
  call f(size(myarray), myarray)

  allocate(mytemp(NEWSIZE))
  mytemp(1:OLDSIZE) = myarray

  flag = .false.
  call foo
  call bar

  deallocate(myarray)
  if (allocated (mytemp)) deallocate (mytemp)

  allocate(myarray(OLDSIZE))
  call f(size(myarray), myarray)

  allocate(mytemp(NEWSIZE))
  mytemp(1:OLDSIZE) = myarray

! Verfify that there is no segfault if the allocatable components
! are deallocated before the call to move_alloc
  flag = .true.
  call foo
  call bar

  deallocate(myarray)
contains
  subroutine foo
    integer :: i
    if (flag) then
      do i = 1, OLDSIZE
        deallocate (mytemp(i)%i)
      end do
    end if
    call move_alloc(mytemp, myarray)
  end subroutine

  subroutine bar
    integer :: i
    do i = 1, OLDSIZE
      if (.not.flag .and. allocated (myarray(i)%i)) then
        if (any (myarray(i)%i .ne. [1,2,3,4,5])) call abort
      else
        if (.not.flag) call abort
      end if
    end do
  end subroutine
end program name
! { dg-final { scan-tree-dump-times "__builtin_malloc" 11 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_free" 11 "original" } }
