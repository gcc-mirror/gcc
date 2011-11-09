! { dg-do compile }
!
! PR fortran/38718
!
  implicit none
  real(kind=8), parameter :: r = kind(0) + 0.2
  complex(kind=8), parameter :: c = (r, -9.3)
  integer, parameter :: k = nint(dreal(c))
  integer, parameter :: l = nint(realpart(c))
  integer(kind=k) :: i
  integer(kind=l) :: j
  i = 42
  j = 42
  print *, k, i, j, r
  end
