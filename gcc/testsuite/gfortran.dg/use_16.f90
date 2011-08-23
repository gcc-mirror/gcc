! { dg-do compile }
!
! PR fortran/31600
!
module a
implicit none
contains
  integer function bar()
    bar = 42
  end function
end module a

use a ! { dg-error "Symbol 'bar' at \\(1\\) conflicts with symbol from module 'a'" }
implicit none
integer :: bar ! { dg-error "Symbol 'bar' at \\(1\\) conflicts with symbol from module 'a'" }
end

! { dg-final { cleanup-modules "a" } }
