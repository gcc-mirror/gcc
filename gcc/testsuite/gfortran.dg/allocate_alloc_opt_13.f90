! { dg-do compile }
!
! PR fortran/51953
!
!
type t
end type t

class(t), allocatable :: a, c(:), e(:)
class(t), pointer :: b, d(:)

allocate (a, b, source=c(1))
allocate (c(4), d(6), source=e)

allocate (a, b, source=f())
allocate (c(1), d(6), source=g())

contains
function f()
  class(t), allocatable :: f
end function
function g()
  class(t), allocatable :: g(:)
end function
end
