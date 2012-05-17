! { dg-do compile }
! { dg-options "-std=f2008" }
!
! Fortran 2008: TYPE ( intrinsic-type-spec )
!
implicit none
type(integer) :: a
type(real) :: b
type(logical ) :: c
type(character) :: d
type(double precision) :: e

type(integer(8)) :: f
type(real(kind=4)) :: g
type(logical ( kind = 1 ) ) :: h
type(character (len=10,kind=1) ) :: i

type(double complex) :: j ! { dg-error "Extension: DOUBLE COMPLEX" }
end

module m
  integer, parameter :: k4  = 4
end module m

type(integer (kind=k4)) function f()
  use m
  f = 42
end
