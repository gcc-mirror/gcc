! { dg-do run }
!
! PR fortran/118613 - check argument evaluation count of MAXVAL

program p
  implicit none
  integer, parameter :: k = 2
  integer :: n
  integer :: i1(k*k), i2(k,k), mm
  real    :: a1(k*k), a2(k,k), mx
  complex :: c1(k*k), c2(k,k)
  logical :: m1(k*k), m2(k,k)

  ! prepare mask for masked variants
  m1 = .true.
  m2 = .true.
  i1 = 0
  i2 = 0
  a1 = 0.
  a2 = 0.
  c1 = 0.
  c2 = 0.

  ! integer
  n = 0
  mm = maxval (h(i1))
  if (n /= k*k .or. mm /= 0) stop 1
  n = 0
  mm = maxval (h(i2))
  if (n /= k*k .or. mm /= 0) stop 2
  n = 0
  mm = maxval (h(i1),m1)
  if (n /= k*k .or. mm /= 0) stop 3
  n = 0
  mm = maxval (h(i2),m2)
  if (n /= k*k .or. mm /= 0) stop 4

  ! real
  n = 0
  mx = maxval (f(a1))
  if (n /= k*k .or. mx /= 0) stop 5
  n = 0
  mx = maxval (f(a2))
  if (n /= k*k .or. mx /= 0) stop 6
  n = 0
  mx = maxval (f(a1),m1)
  if (n /= k*k .or. mx /= 0) stop 7
  n = 0
  mx = maxval (f(a2),m2)
  if (n /= k*k .or. mx /= 0) stop 8

  ! complex
  n = 0
  mx = maxval (g(c1))
  if (n /= k*k .or. mx /= 0) stop 9
  n = 0
  mx = maxval (g(c2))
  if (n /= k*k .or. mx /= 0) stop 10
  n = 0
  mx = maxval (g(c1),m1)
  if (n /= k*k .or. mx /= 0) stop 11
  n = 0
  mx = maxval (g(c2),m2)
  if (n /= k*k .or. mx /= 0) stop 12

contains

  impure elemental function h (x)
    integer, intent(in) :: x
    integer             :: h
    h = abs (x)
    n = n + 1   ! Count number of function evaluations
  end

  impure elemental function f (x)
    real, intent(in) :: x
    real             :: f
    f = abs (x)
    n = n + 1   ! Count number of function evaluations
  end

  impure elemental function g (x)
    complex, intent(in) :: x
    real                :: g
    g = abs (x)
    n = n + 1   ! Count number of function evaluations
  end
end
