! { dg-do run }
!
! PR fortran/115150
!
! Check that SHAPE handles zero-sized arrays correctly
!
implicit none
call one
call two

contains

subroutine one
  real,allocatable :: A(:),B(:,:)
  allocate(a(3:0), b(5:1, 2:5))

  if (any (shape(a) /= [0])) stop 1
  if (any (shape(b) /= [0, 4])) stop 2
  if (size(a) /= 0) stop 3
  if (size(b) /= 0) stop 4
  if (any (lbound(a) /= [1])) stop 5
  if (any (lbound(b) /= [1, 2])) stop 6
  if (any (ubound(a) /= [0])) stop 5
  if (any (ubound(b) /= [0,5])) stop 6
end

subroutine two
integer :: x1(10), x2(10,10)
call f(x1, x2, -3)
end

subroutine f(y1, y2, n)
  integer, value :: n
  integer :: y1(1:n)
  integer :: y2(1:n,4,2:*)
  call g(y1, y2)
end

subroutine g(z1, z2)
  integer :: z1(..), z2(..)

  if (any (shape(z1) /= [0])) stop 1
  if (any (shape(z2) /= [0, 4, -1])) stop 2
  if (size(z1) /= 0) stop 3
  if (size(z2) /= 0) stop 4
  if (any (lbound(z1) /= [1])) stop 5
  if (any (lbound(z2) /= [1, 1, 1])) stop 6
  if (any (ubound(z1) /= [0])) stop 5
  if (any (ubound(z2) /= [0, 4, -1])) stop 6
end
end
