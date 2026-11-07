! { dg-do run }
! ICE: segfault on ALLOCATE with an assumed (*) type-param-value.
! F2023 C939 makes this the ONLY legal use of * in an ALLOCATE type-spec.
! This is the same as PR82649.
module bug3
  type :: t(n)
    integer, len :: n = 1
    real :: v(n)
  end type

  type, extends(t) :: u(m)
    integer, len :: m = 1
    real :: mat (n,m)
  end type
contains

  subroutine fill (a)
    type(t(*)), allocatable, intent(out) :: a
    integer :: i
    allocate (t(*) :: a) ! Seg fault at trans-decl.cc:1697
    A%V = [(REAL (i, kind = kind(1.0)), i = 1, a%n)]
  end subroutine

  subroutine optional_fill (a)
    type(t(*)), allocatable, optional :: a
    integer :: i
    if (present (a)) then
      if (allocated (a)) then
        if (int (sum (a%v)) /= 10) stop 1001
        deallocate (a)
      end if
      allocate (t(*) :: a)
      A%V = [(REAL (2 * i, kind = kind(1.0)), i = 1, a%n)]
    endif
  end subroutine

  subroutine cfill (a)
    class(t(*)), allocatable, intent(out) :: a
    integer :: i
    allocate (t(*) :: a)
    A%V = [(REAL (3 * i, kind = kind(1.0)), i = 1, a%n)]
  end subroutine

  subroutine cmfill (a)
    class(t(*)), allocatable, intent(out) :: a
    integer :: i
    allocate (u(*,4) :: a)
    A%V = [(REAL (4 * i, kind = kind(1.0)), i = 1, a%n)]
    select type (a)
      type is (u(*,*))
        a%mat = 1
    end select
  end subroutine

  subroutine array_fill (a)
    type(t(*)), allocatable, intent(out) :: a(:)
    integer :: i
    allocate (t(*) :: a(2))
    A(2)%V = [(REAL (5 * i, kind = kind(1.0)), i = 1, a%n)]
  end subroutine

  subroutine array_optional_fill (a)
    type(t(*)), allocatable, optional :: a(:)
    integer :: i
    if (present (a)) then
      if (allocated (a)) then
        if (int (sum (a(2)%v)) /= 50) stop 1002
        deallocate (a)
      end if
      allocate (t(*) :: a(2))
      A(2)%V = [(REAL (6 * i, kind = kind(1.0)), i = 1, a%n)]
    endif
  end subroutine

  subroutine array_cfill (a)
    class(t(*)), allocatable, intent(out) :: a(:)
    integer :: i
    allocate (t(*) :: a(4))
    a(2)%V = [(REAL (6 * i, kind = kind(1.0)), i = 1, a(1)%n)]
  end subroutine

  subroutine array_cmfill (aaa)
    class(t(*)), allocatable, intent(out) :: aaa(:)
    integer :: i
    allocate (u(*,4) :: aaa(2)) ! Seg fault at trans-decl.cc:1697
    aaa(2)%V = [(REAL (7 * i, kind = kind(1.0)), i = 1, aaa(1)%n)]
    select type (aaa)
      type is (u(*,*))
        aaa(2)%mat = 2
    end select
  end subroutine

end module

! Module for pr82649
module matrix_mod_assumed_02

  implicit none

  type :: matrix(rk, n, m)
     integer, kind :: rk
     integer, len :: n, m
     real(rk) :: entry(n, m)
  end type matrix
  integer, parameter :: rk=kind(1.d0)
  integer, parameter :: mm=20, nn=15

contains
  subroutine factory(o_matrix)
    type(matrix(rk, *, *)), allocatable :: o_matrix
    allocate(matrix(rk, *, *) :: o_matrix)
    o_matrix%entry = 1.0
  end subroutine

  subroutine indirect_factory(o_matrix)
    type(matrix(rk, *, *)), allocatable :: o_matrix
    call factory(o_matrix)
    o_matrix%entry = 2.0
  end subroutine
end module

program bug3_allocate_star_ice

  use bug3
  type(t(4)), allocatable :: x
  type(t(4)), allocatable :: xa(:)
  class(t(4)), allocatable :: y
  class(t(4)), allocatable :: ya(:)

! First scalars
  call fill(x)                         ! Original test
  IF (size (x%v) /= 4) STOP 2
  if (int (sum (x%v)) /= 10) stop 3
  call optional_fill (x)               ! With optional argument
  if (int (sum (x%v)) /= 20) stop 4
  deallocate (x)
  call optional_fill ()                ! With missing optional argument
  if (allocated (x)) stop 5
  call cfill (y)                       ! Class version of original
  if (int (sum (y%v)) /= 30) stop 6
  deallocate (y)
  call cmfill (y)                      ! Class with type extension
  select type (y)
    type is (u(*,*))
        if (int (sum (y%v)) /= 40) stop 7
        if (int (sum (y%mat)) /= 16) stop 8
  end select
  deallocate(y)

! Now check arrays
  call array_fill(xa)                  ! Array version of original test
  if (xa%n /= 4) stop 9
  if (size (xa(1)%v, 1) /= 4) stop 10
  if (int (sum (xa(2)%v)) /= 50) stop 11
  call array_optional_fill (xa)        ! With optional array argument
  if (size (xa(1)%v, 1) /= 4) stop 12
  if (int (sum (xa(2)%v)) /= 60) stop 13
  deallocate (xa)
  call  array_optional_fill ()         ! With missing optional array argument
  if (allocated (xa)) stop 14
  call array_cfill(ya)                 ! Class array version of original
  if (ya(1)%n /= 4) stop 15
  if (size (ya(1)%v, 1) /= 4) stop 16
  if (int (sum (ya(2)%v)) /= 60) stop 17
  deallocate (ya)
  call array_cmfill (ya)                ! Class array with type extension
  select type (ya)
    type is (u(*,*))
        if (int (sum (ya(2)%v)) /= 70) print *,int (sum (ya(2)%v))
        if (int (sum (ya(2)%mat)) /= 32) print *,int (sum (ya(2)%mat))
  end select
  deallocate (ya)
! Check PR82649 - both original and with type parameters transmitted
! Through an indirect call.
  call pr82649

contains

  subroutine pr82649
    use matrix_mod_assumed_02
    implicit none
    type(matrix(rk, nn, mm)), allocatable  :: o_matrix

    call factory(o_matrix)
    if (o_matrix%n /= nn .or. o_matrix%m /= mm) stop 101
    if (int (sum (o_matrix%entry)) /= nn * mm) stop 102
    deallocate (o_matrix)

    call indirect_factory(o_matrix)
    if (o_matrix%n /= nn .or. o_matrix%m /= mm) stop 103
    if (int (sum (o_matrix%entry)) /= 2 * nn * mm) stop 104
    deallocate (o_matrix)
  end subroutine

end
