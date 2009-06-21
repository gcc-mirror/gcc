! { dg-do compile }
! PR33162 INTRINSIC functions as ACTUAL argument
! Test case adapted from PR by Jerry DeLisle <jvdelisle@gcc.gnu.org>
module m
implicit none
contains
  subroutine sub(a)
    interface
      function a()
        real :: a
      end function a
    end interface
    print *, a()
  end subroutine sub
end module m
use m
implicit none
intrinsic cos
call sub(cos) ! { dg-error "wrong number of arguments" }
end

! { dg-final { cleanup-modules "m" } }

