! { dg-do compile }
module test
  interface operator(.bar.)
     module procedure func
  end interface
contains
function func(a)
  integer,intent(in) :: a
  integer :: funct
  func = a+1
end function
end module test

use test, only: operator(.func.) ! { dg-error "not found in module 'test'" }
end
! { dg-final { cleanup-modules "test" } }
