! { dg-do run }
! { dg-additional-sources assumed_rank_8_c.c }
!
! PR fortran/48820
!
! Scalars to assumed-rank tests
!
program main
  implicit none

  type t
    integer :: i
  end type t

  interface
    subroutine check (x)
      integer :: x(..)
    end subroutine check
    subroutine check2 (x)
      import t
      class(t) :: x(..)
    end subroutine check2
  end interface

  integer :: j

  type(t), target :: y
  class(t), allocatable, target :: yac
  
  y%i = 489
  allocate (yac)
  yac%i = 489
  j = 0
  call fc()
  call fc(null())
  call fc(y)
  call fc(yac)
  if (j /= 2) STOP 1

  j = 0
  call gc(null())
  call gc(y)
  call gc(yac)
  deallocate (yac)
  call gc(yac)
  if (j /= 2) STOP 2

  j = 0
  call hc(yac)
  allocate (yac)
  yac%i = 489
  call hc(yac)
  if (j /= 1) STOP 3

  j = 0
  call ft()
  call ft(null())
  call ft(y)
  call ft(yac)
  if (j /= 2) STOP 4

  j = 0
  call gt(null())
  call gt(y)
  call gt(yac)
  deallocate (yac)
  call gt(yac)
  if (j /= 2) STOP 5

  j = 0
  call ht(yac)
  allocate (yac)
  yac%i = 489
  call ht(yac)
  if (j /= 1) STOP 6

contains

  subroutine fc (x)
    class(t), optional :: x(..)

    if (.not. present (x)) return
    if (.not. SAME_TYPE_AS (x, yac)) STOP 7
    if (rank (x) /= 0) STOP 1
    call check2 (x)
    j = j + 1
  end subroutine

  subroutine gc (x)
    class(t), pointer, intent(in) :: x(..)

    if (.not. associated (x)) return
    if (.not. SAME_TYPE_AS (x, yac)) STOP 8
    if (rank (x) /= 0) STOP 9
    call check2 (x)
    j = j + 1
  end subroutine

  subroutine hc (x)
    class(t), allocatable :: x(..)

    if (.not. allocated (x)) return
    if (.not. SAME_TYPE_AS (x, yac)) STOP 10
    if (rank (x) /= 0) STOP 2
    call check2 (x)
    j = j + 1
  end subroutine

  subroutine ft (x)
    type(t), optional :: x(..)

    if (.not. present (x)) return
    if (.not. SAME_TYPE_AS (x, yac)) STOP 11
    if (rank (x) /= 0) STOP 3
    call check2 (x)
    j = j + 1
  end subroutine

  subroutine gt (x)
    type(t), pointer, intent(in) :: x(..)

    if (.not. associated (x)) return
    if (.not. SAME_TYPE_AS (x, yac)) STOP 12
    if (rank (x) /= 0) STOP 13
    call check2 (x)
    j = j + 1
  end subroutine

  subroutine ht (x)
    type(t), allocatable :: x(..)

    if (.not. allocated (x)) return
    if (.not. SAME_TYPE_AS (x, yac)) STOP 14
    if (rank (x) /= 0) STOP 4
    call check2 (x)
    j = j + 1
  end subroutine

end program main
