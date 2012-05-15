! { dg-do compile }
! we used to save the wrong components of a gfc_expr describing a
! substring of a constant string.  This yielded a segfault on
! translating the expressions read from the module.
module m
  character (*), parameter :: a = "AABBCC"(1:4)
end module m

use m
character(4) :: b
b = a
end
