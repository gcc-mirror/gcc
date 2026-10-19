! { dg-do run }
! See attachment to PR82173 comment 10 - reproducers/bug21-assumed-dummy-corrupts-inherited-param.f90
module base4_m
  type :: base(k, n)
    integer, kind :: k = 4
    integer, len :: n = 1
    real(k) :: b(n)
  end type
end module
module ext4_m
  use base4_m
  type, extends(base) :: ext(m)
    integer, len :: m = 1
    real(k) :: e(n + m)
  end type
contains
  integer function esize (a)
    type(ext(4, *, *)), intent(in) :: a
    esize = size (a%e)
  end function
end module
program u4
  use ext4_m
  type(ext(4, 2, 3)) :: x
  x%b = 1.0
  x%e = 2.0
  if (x%k /= 4) stop 1
  if (x%n /= 2) stop 2
  if (x%m /= 3) stop 3
  if (esize (x) /= 5) stop 4
  if (int (sum (x%b)) /= x%n) stop 5
  if (int (sum (x%e)) /= 2 * (x%n + x%m)) stop 6
end program
