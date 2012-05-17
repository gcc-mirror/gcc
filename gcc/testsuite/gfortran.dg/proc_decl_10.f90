! { dg-do compile }
! PR33162 INTRINSIC functions as ACTUAL argument
! Test case adapted from PR by Jerry DeLisle <jvdelisle@gcc.gnu.org>
module m
implicit none
  interface
    double precision function my1(x)
      double precision, intent(in) :: x
    end function my1
  end interface
  interface
    real(kind=4) function my2(x)
      real, intent(in) :: x
    end function my2
  end interface
  interface
    real function  my3(x, y)
      real, intent(in) :: x, y
    end function my3
  end interface
end module

program test
use m
implicit none
procedure(dcos):: my1 ! { dg-error "Cannot change attributes" }
procedure(cos) :: my2 ! { dg-error "Cannot change attributes" }
procedure(dprod) :: my3 ! { dg-error "Cannot change attributes" }

end program test
