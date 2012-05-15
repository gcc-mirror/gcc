! { dg-do run }
! PR fortan/31692
! Passing array valued results to procedures
!
! Test case contributed by rakuen_himawari@yahoo.co.jp
module one
  integer :: flag = 0
contains
  function foo1 (n)
    integer :: n
    integer :: foo1(n)
    if (flag == 0) then
      call bar1 (n, foo1)
    else
      call bar2 (n, foo1)
    end if
  end function

  function foo2 (n)
    implicit none
    integer :: n
    integer,ALLOCATABLE :: foo2(:)
    allocate (foo2(n))
    if (flag == 0) then
      call bar1 (n, foo2)
    else
      call bar2 (n, foo2)
    end if
  end function

  function foo3 (n)
    implicit none
    integer :: n
    integer,ALLOCATABLE :: foo3(:)
    allocate (foo3(n))
    foo3 = 0
    call bar2(n, foo3(2:(n-1)))  ! Check that sections are OK
  end function

  subroutine bar1 (n, array)     ! Checks assumed size formal arg.
    integer :: n
    integer :: array(*)
    integer :: i
    do i = 1, n
      array(i) = i
    enddo
  end subroutine

  subroutine bar2(n, array)     ! Checks assumed shape formal arg.
    integer :: n
    integer :: array(:)
    integer :: i
    do i = 1, size (array, 1)
      array(i) = i
    enddo
   end subroutine
end module

program main
  use one
  integer :: n
  n = 3
  if(any (foo1(n) /= [ 1,2,3 ])) call abort()
  if(any (foo2(n) /= [ 1,2,3 ])) call abort()
  flag = 1
  if(any (foo1(n) /= [ 1,2,3 ])) call abort()
  if(any (foo2(n) /= [ 1,2,3 ])) call abort()
  n = 5
  if(any (foo3(n) /= [ 0,1,2,3,0 ])) call abort()
end program
