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
  call f (null())
  call f (kk)
  if (j /= 2) call abort()

  j = 0
  nullify (ll)
  call g (null())
  call g (ll)
  call g (ii)
  if (j /= 1) call abort()

  j = 0
  call h (kk)
  kk = 489
  call h (kk)
  if (j /= 1) call abort()

contains

  subroutine f (x)
    integer, optional :: x(..)

    if (.not. present (x)) return
    if (rank (x) /= 0) call abort
    call check (x)
    j = j + 1
  end subroutine

  subroutine g (x)
    integer, pointer, intent(in) :: x(..)

    if (.not. associated (x)) return
    if (rank (x) /= 0) call abort ()
    call check (x)
    j = j + 1
  end subroutine

  subroutine h (x)
    integer, allocatable :: x(..)

    if (.not. allocated (x)) return
    if (rank (x) /= 0) call abort
    call check (x)
    j = j + 1
  end subroutine

end program main
