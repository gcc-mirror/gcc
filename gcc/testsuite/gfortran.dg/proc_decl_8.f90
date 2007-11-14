! { dg-do compile }
! PR33162 INTRINSIC functions as ACTUAL argument
! Test case adapted from PR by Jerry DeLisle <jvdelisle@gcc.gnu.org>
module m
implicit none
contains
  subroutine sub(a)
    interface
      function a(x)
        real :: a, x
        intent(in) :: x
      end function a
    end interface
    print *, a(4.0)
  end subroutine sub

end module m

use m
implicit none
EXTERNAL foo  ! interface is undefined
procedure(cos) :: foo ! { dg-error "Duplicate EXTERNAL attribute specified" }
call sub(foo)         ! { dg-error "Type/rank mismatch in argument" }
end
! { dg-final { cleanup-modules "m" } }
