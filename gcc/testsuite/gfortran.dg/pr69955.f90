! { dg-do run }
! { dg-options "-fdump-tree-original" }

program p
  implicit none

  type :: t1
    integer, allocatable :: t(:)
  end type t1

  type :: t2
    type(t1), allocatable :: x1(:)
  end type t2

  type(t2) :: var(10)

  integer :: i

  do i= 1, 10
    allocate(var(i)%x1(100))
    allocate(var(i)%x1(1)%t(100))
  enddo

  open(unit = 37, file = "/dev/null", status = "old")

  call s(1)

  close(unit = 37)

  do i=1,10
    deallocate(var(i)%x1)
  enddo

contains

  subroutine s(counter)
    implicit none
    integer, intent(in) :: counter
    integer :: i, j, n

    do j=1, counter
      n = size( [ ( var(i)%x1 , i = 1, size(var) ) ] )
      write(unit = 37, fmt = '(i5)') n
    enddo
  end subroutine

end program p
! { dg-final { scan-tree-dump-times "__builtin_malloc" 4 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_free" 4 "original" } }
