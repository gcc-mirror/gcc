! { dg-do run }
! reduced testcase from PR 17740
module FOO

  interface BAR
     module procedure BAR2
  end interface

contains

  elemental integer function BAR2(X)
    integer, intent(in) :: X
    BAR2 = X
  end function

  subroutine BAZ(y,z)
    integer :: Y(3), Z(3)
    Z = BAR(Y)
  end subroutine

end module

use foo
integer :: y(3), z(3)
y = (/1,2,3/)
call baz(y,z)
if (any (y /= z)) call abort ()
end
