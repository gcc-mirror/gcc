! { dg-do compile }
! PR fortran/106500 - fix checking of arguments to C_SIZEOF
!
! Check support of the following EDIT to 18-007r1:
! https://j3-fortran.org/doc/year/22/22-101r1.txt

subroutine foo (n, x, y, z, w, u)
  use, intrinsic :: iso_c_binding
  implicit none
  integer, intent(in) :: n
  real :: x(n)
  real :: y(:)
  real :: z(2,*)
  real :: w(..)
  real, allocatable :: a(:)
  real, pointer     :: b(:)
  type t
     real, allocatable :: a(:)
  end type t
  type(t) :: u

  print *, c_sizeof (x)
  print *, c_sizeof (x(::2))
  print *, c_sizeof (x+1)
  print *, c_sizeof (y)
  print *, c_sizeof (y(1:2))
  print *, c_sizeof (z(:,1:2))
  print *, c_sizeof (w)
  print *, c_sizeof (1._c_float)
  !
  allocate (a(n))
  allocate (b(n))
  if (.not. allocated (u%a)) allocate (u%a(n))
  print *, c_sizeof (a)
  print *, c_sizeof (b)
  !
  print *, c_sizeof (u%a)
  print *, c_sizeof (u%a(1:2))
  !
  print *, c_sizeof (z) ! { dg-error "Assumed-size arrays are not interoperable" }
  print *, c_sizeof (u) ! { dg-error "Expression is a noninteroperable derived type" }
end
