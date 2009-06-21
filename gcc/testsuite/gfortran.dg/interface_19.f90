! { dg-do run }
! PR33162 INTRINSIC functions as ACTUAL argument
! Test case adapted from PR by Jerry DeLisle <jvdelisle@gcc.gnu.org>
module m
implicit none
contains
  subroutine sub(a)
    optional :: a
    character(25) :: temp
    interface
      function a(x)
        real(kind=8):: a
        real(kind=8):: x
        intent(in) :: x
      end function a
    end interface
    if(present(a)) then
      write(temp,'(f16.10)')a(4.0d0)
      if (trim(temp) /= '   -0.6536436209') call abort
    endif
  end subroutine sub
end module m

use m
implicit none
intrinsic dcos
call sub()
call sub(dcos)
end

! { dg-final { cleanup-modules "m" } }

