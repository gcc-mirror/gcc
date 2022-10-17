! { dg-do compile }
! { dg-options "-fdump-tree-original" }
! { dg-final { scan-tree-dump-times "_gfortran_stop_numeric" 0 "original" } }
! PR fortran/102458

subroutine s4
  integer, parameter :: n = 4
  integer            :: w(transfer(n, n)) = 1
  integer            :: x(transfer(n, n))
  integer            :: y(2*int(n) - n)
  type t
     integer         :: z(int(n))
  end type t
  type(t)            :: tt, uu(3)
  integer, parameter :: i = size (w)
  integer, parameter :: k = size (x)
  integer, parameter :: m = size (y)
  integer, parameter :: j = size (tt% z)
  integer, parameter :: l = size (uu(2)% z)
  if (i /= n .or. k /= n .or. m /= n .or. j /= n .or. l /= n) stop 1
end
