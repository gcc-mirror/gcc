! { dg-do run }
! { dg-additional-sources assumed_rank_8_c.c }
!
! PR fortran/48820
!
! Scalars to assumed-rank tests
!
program main
  implicit none

  interface
    subroutine check (x)
      integer :: x(..)
    end subroutine check
  end interface

  integer, target :: ii, j
  integer, allocatable :: kk
  integer, pointer :: ll
  ii = 489
  j = 0
  call f (ii)
  call f (489)
  call f ()
  call f (null(kk))
  call f (kk)
  if (j /= 2) STOP 1

  j = 0
  nullify (ll)
  call g (null(ll))
  call g (ll)
  call g (ii)
  if (j /= 1) STOP 2

  j = 0
  call h (kk)
  kk = 489
  call h (kk)
  if (j /= 1) STOP 3

contains

  subroutine f (x)
    integer, optional :: x(..)

    if (.not. present (x)) return
    if (rank (x) /= 0) STOP 1
    call check (x)
    j = j + 1
  end subroutine

  subroutine g (x)
    integer, pointer, intent(in) :: x(..)

    if (.not. associated (x)) return
    if (rank (x) /= 0) STOP 4
    call check (x)
    j = j + 1
  end subroutine

  subroutine h (x)
    integer, allocatable :: x(..)

    if (.not. allocated (x)) return
    if (rank (x) /= 0) STOP 2
    call check (x)
    j = j + 1
  end subroutine

end program main
