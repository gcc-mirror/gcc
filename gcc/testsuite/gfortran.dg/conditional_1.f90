! { dg-do run }
! { dg-options "-std=f2023" }
program conditional_simple
  implicit none
  integer :: i = 42
  logical :: l = .true.
  real(4) :: r1 = 1.e-4, r2 = 1.e-5
  complex :: z = (3.0, 4.0)
  character(kind=1, len=5) :: c1 = "hello", c2 = "world"
  character(len=:), allocatable :: c3

  i = (i > 0 ? 1 : -1)
  if (i /= 1) stop 1

  i = 0
  i = (i > 0 ? 1 : i < 0 ? -1 : 0)
  if (i /= 0) stop 2

  i = 0
  i = (i > 0 ? 1 : (i < 0 ? -1 : 0))
  if (i /= 0) stop 3

  i = 0
  i = (l .eqv. .false. ? 1 : 0)
  if (i /= 0) stop 4

  i = 0
  i = (r1 /= r2 ? 0 : 1)
  if (i /= 0) stop 5

  i = 0
  z = (i /= 0 ? z : (-3.0, -4.0))
  if (z /= (-3.0, -4.0)) stop 6

  i = 0
  c1 = (i /= 0 ? c1 : c2)
  if (c1 /= "world") stop 7

  i = 0
  c1 = (i /= 0 ? "abcde" : "bcdef")
  if (c1 /= "bcdef") stop 8

  i = 0
  c3 = (i /= 0 ? "abcde" : c2(1:3))
  if (c3 /= "wor") stop 9
end program conditional_simple
