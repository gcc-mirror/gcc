! { dg-do compile }
! PR fortran/56423
!
! Check constraints on pointer targets for derived type constructors
!
! Contributed by Tobias Burnus and Gerhard Steinmetz

program p
  integer, target :: x(3) = [7, 8, 9]
  type t
     integer, pointer :: a(:)
  end type
  type(t) :: z
  z = t(x)
  z = t(x(1:3))
  z = t(x(3:1:-1))
  z = t(x(2))     ! { dg-error "rank of the element in the structure constructor" }
  z = t(x([1,3])) ! { dg-error "has a vector subscript" }
  print *, z%a
end
