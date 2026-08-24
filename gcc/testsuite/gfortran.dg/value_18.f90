! { dg-do run }
! PR 49802
! An absent OPTIONAL actual argument passed to an OPTIONAL dummy with the
! VALUE attribute used to segfault: the private copy made for the dummy
! dereferenced the actual argument unconditionally.  Both the array copy
! and the copy made for a character dummy of assumed or non-constant
! length must be suppressed when the actual argument is absent.

module m
  implicit none
  type :: dt
    integer, allocatable :: d(:)
  end type
contains

  subroutine take_as (x, present_x)
    integer, value, optional :: x(:)
    logical, intent(in) :: present_x
    if (present (x) .neqv. present_x) stop 1
    if (present (x)) then
      if (size (x) /= 3) stop 2
      if (any (x /= [1, 2, 3])) stop 3
      x = -1
      if (any (x /= -1)) stop 4
    end if
  end subroutine take_as

  subroutine take_es (n, x, present_x)
    integer, intent(in) :: n
    integer, value, optional :: x(n)
    logical, intent(in) :: present_x
    if (present (x) .neqv. present_x) stop 5
    if (present (x)) then
      if (size (x) /= n) stop 6
      if (x(1) /= 1) stop 7
      x = 0
      if (any (x /= 0)) stop 8
    end if
  end subroutine take_es

  subroutine take_cs (s, present_s)
    character(len=*), value, optional :: s
    logical, intent(in) :: present_s
    if (present (s) .neqv. present_s) stop 9
    if (present (s)) then
      if (s /= 'payload') stop 10
      s = repeat ('Z', len (s))
      if (s /= repeat ('Z', len (s))) stop 11
    end if
  end subroutine take_cs

  subroutine take_ca (s, present_s)
    character(len=*), value, optional :: s(:)
    logical, intent(in) :: present_s
    if (present (s) .neqv. present_s) stop 12
    if (present (s)) then
      if (size (s) /= 2) stop 13
      if (any (s /= ['ab', 'cd'])) stop 14
      s = 'ZZ'
      if (any (s /= 'ZZ')) stop 15
    end if
  end subroutine take_ca

  ! A derived type with an allocatable component: the deep copy must be
  ! suppressed too, not just the copy of the descriptor.

  subroutine take_dt (x, present_x)
    type(dt), value, optional :: x(:)
    logical, intent(in) :: present_x
    if (present (x) .neqv. present_x) stop 19
    if (present (x)) then
      if (size (x) /= 2) stop 20
      if (any (x(1)%d /= [1, 1])) stop 21
      x(1)%d = [-1, -1]
      if (any (x(1)%d /= [-1, -1])) stop 22
    end if
  end subroutine take_dt

  ! Relay an optional dummy on to the optional VALUE dummy.  This is what
  ! puts a descriptor of an absent argument into the argument list.

  subroutine relay_as (x, present_x)
    integer, optional :: x(:)
    logical, intent(in) :: present_x
    call take_as (x, present_x)
  end subroutine relay_as

  subroutine relay_es (n, x, present_x)
    integer, intent(in) :: n
    integer, optional :: x(n)
    logical, intent(in) :: present_x
    call take_es (n, x, present_x)
  end subroutine relay_es

  ! A non-constant length is needed here: with len=* the length of an
  ! absent actual argument is zero and the copy reads nothing.
  subroutine relay_cs (n, s, present_s)
    integer, intent(in) :: n
    character(len=n), optional :: s
    logical, intent(in) :: present_s
    call take_cs (s, present_s)
  end subroutine relay_cs

  subroutine relay_ca (s, present_s)
    character(len=*), optional :: s(:)
    logical, intent(in) :: present_s
    call take_ca (s, present_s)
  end subroutine relay_ca

  subroutine relay_dt (x, present_x)
    type(dt), optional :: x(:)
    logical, intent(in) :: present_x
    call take_dt (x, present_x)
  end subroutine relay_dt

  ! An optional VALUE dummy relayed on to another optional VALUE dummy.

  subroutine relay_asv (x, present_x)
    integer, value, optional :: x(:)
    logical, intent(in) :: present_x
    call take_as (x, present_x)
  end subroutine relay_asv

  subroutine relay_csv (n, s, present_s)
    integer, intent(in) :: n
    character(len=n), value, optional :: s
    logical, intent(in) :: present_s
    call take_cs (s, present_s)
  end subroutine relay_csv

end module m

program test
  use m
  implicit none
  integer :: v(3), i
  character(len=7) :: s
  character(len=2) :: a(2)
  type(dt) :: w(2)

  v = [1, 2, 3]
  s = 'payload'
  a = ['ab', 'cd']
  do i = 1, 2
    allocate (w(i)%d(2), source=[i, i])
  end do

  ! Directly, with and without the actual argument.
  call take_as (v, .true.)
  call take_as (present_x = .false.)
  call take_es (3, v, .true.)
  call take_es (3, present_x = .false.)
  call take_cs (s, .true.)
  call take_cs (present_s = .false.)
  call take_ca (a, .true.)
  call take_ca (present_s = .false.)
  call take_dt (w, .true.)
  call take_dt (present_x = .false.)

  ! Relayed through an optional dummy.
  call relay_as (v, .true.)
  call relay_as (present_x = .false.)
  call relay_es (3, v, .true.)
  call relay_es (3, present_x = .false.)
  call relay_cs (7, s, .true.)
  call relay_cs (7, present_s = .false.)
  call relay_ca (a, .true.)
  call relay_ca (present_s = .false.)
  call relay_dt (w, .true.)
  call relay_dt (present_x = .false.)

  ! Relayed through an optional VALUE dummy.
  call relay_asv (v, .true.)
  call relay_asv (present_x = .false.)
  call relay_csv (7, s, .true.)
  call relay_csv (7, present_s = .false.)

  ! None of the copies may write back to the actual arguments.
  if (any (v /= [1, 2, 3])) stop 16
  if (s /= 'payload') stop 17
  if (any (a /= ['ab', 'cd'])) stop 18
  if (any (w(1)%d /= [1, 1])) stop 23
  if (any (w(2)%d /= [2, 2])) stop 24
end program test
