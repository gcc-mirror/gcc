! { dg-do run }
! { dg-require-effective-target fortran_integer_16 }
! { dg-require-effective-target fortran_real_16 }
! { dg-additional-options "-fdump-tree-original" }
! { dg-final { scan-tree-dump-times "_gfortran_stop_numeric" 2 "original" } }
!
! PR fortran/96711 - ICE on NINT() Function

program p
  implicit none
  real(8)                :: x
  real(16)               :: y
  ! Assume radix(x) == 2
  ! 2/epsilon(x) = 2/(radix(x)**(1-digits(x)) = 2**digits(x) with that assumption
  integer(16), parameter :: k1 = nint (2 / epsilon (x), kind(k1))
  integer(16), parameter :: k2 = nint (2 / epsilon (y), kind(k2))
  integer(16), parameter :: m1 = 2_16**digits(x) ! IEEE: 2**53
  integer(16), parameter :: m2 = 2_16**digits(y) ! IEEE: 2**113
  integer(16), volatile  :: m
  x = 2 / epsilon (x)
  y = 2 / epsilon (y)
  m = nint (x, kind(m))
! print *, m
  if (k1 /= m1) stop 1
  if (m  /= m1) stop 2
  m = nint (y, kind(m))
! print *, m
  if (k2 /= m2) stop 3
  if (m  /= m2) stop 4
end program
