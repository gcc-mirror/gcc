! { dg-do run }
! Sourced from comment #10 of PR82173.
! This test brings together ../pos/p67_N_array_intrinsics.f90:
!       Array intrinsics applied to arrays of parameterized derived type.
! and ../pos/p66_N_transfer.f90
!       F2023 16.9.193: TRANSFER with a parameterized derived type source.
! Valgrind shows no errors or memory leaks.
! 
module m_p67
  implicit none
  type :: t(n)
    integer, len :: n = 1
    integer :: v(n)
    integer :: tag
  end type
end module
module m_p66
  implicit none
  type :: t(n)
    integer, len :: n = 1
    integer :: v(n)
  end type
end module

  call p67
  call p66
contains
subroutine p67
  use m_p67
  implicit none
  type(t(2)) :: a(6), b(2, 3), c(6), one
  logical :: mask(6)
  integer :: i

  do i = 1, 6
    a(i)%v = i
    a(i)%tag = i
  end do
  one%v = 0
  one%tag = 99

  ! RESHAPE
  b = reshape (a, [2, 3])
  if (size (b, 1) /= 2 .or. size (b, 2) /= 3) error stop 1
  if (b(1, 1)%tag /= 1 .or. b(2, 3)%tag /= 6) error stop 2
  if (b(2, 1)%n /= 2) error stop 3

  ! PACK
  mask = [.true., .false., .true., .false., .true., .false.]
  c(1:3) = pack (a, mask)
  if (c(1)%tag /= 1 .or. c(2)%tag /= 3 .or. c(3)%tag /= 5) error stop 4

  ! SPREAD
  if (size (spread (one, 1, 4)) /= 4) error stop 5

  ! CSHIFT / EOSHIFT
  c = cshift (a, 2)
  if (c(1)%tag /= 3 .or. c(6)%tag /= 2) error stop 6
  c = eoshift (a, 1, one)
  if (c(6)%tag /= 99) error stop 7

  ! MERGE
  c = merge (a, a(6:1:-1), mask)
  if (c(1)%tag /= 1 .or. c(2)%tag /= 5) error stop 8
end subroutine

subroutine p66
  use m_p66
  implicit none
  type(t(3)) :: a, b
  integer, allocatable :: buf(:)

  a%v = [11, 22, 33]

  buf = transfer (a, [0])
  if (size (buf) < 3) error stop 101

  b = transfer (buf, b)
  if (any (b%v /= [11, 22, 33])) stop 102
  if (b%n /= 3) error stop 103
  if (allocated (buf)) deallocate (buf)
end subroutine

end
