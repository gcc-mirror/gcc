! { dg-do run }
! PR41807  data statement with nested type constructors
! Test case provided by Steve Kargl
  implicit none

  type :: a
     real :: x(3)
  end type a

  integer, parameter :: n = 3

  type(a) :: b(n)

  real, parameter :: d1(3) = (/1., 2., 3./)
  real, parameter :: d2(3) = (/4., 5., 6./)
  real, parameter :: d3(3) = (/7., 8., 9./)

  integer :: i, z(n)
 
  data (b(i), i = 1, n) /a(d1), a(d2), a(d3)/
  data (z(i), i = 1, n) / 1, 2, 3/

  if (any(z.ne.[1, 2, 3])) STOP 1
  if (any(b(1)%x.ne.[1, 2, 3]) .or. &
      any(b(2)%x.ne.[4, 5, 6]) .or. &
      any(b(3)%x.ne.[7, 8, 9])) STOP 2
end

