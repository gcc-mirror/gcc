! { dg-do compile }
!
! PR fortran/58099
!
! See also interpretation request F03-0130 in 09-217 and 10-006T5r1.
!
! - ELEMENTAL is only permitted for external names with PROCEDURE/INTERFACE
!   but not for dummy arguments or proc-pointers
! - Using PROCEDURE with an elemental intrinsic as interface name a is valid,
! but doesn't make the proc-pointer/dummy argument elemental
!

  interface
    elemental real function x(y)
      real, intent(in) :: y
    end function x
  end interface
  intrinsic :: sin
  procedure(x) :: xx1 ! OK
  procedure(x), pointer :: xx2 ! { dg-error "Procedure pointer 'xx2' at .1. shall not be elemental" }
  procedure(real), pointer :: pp 
  procedure(sin) :: bar ! OK
  procedure(sin), pointer :: foo ! { dg-error "Procedure pointer 'foo' at .1. shall not be elemental" }
  pp => sin !OK
contains
  subroutine sub1(z) ! { dg-error "Dummy procedure 'z' at .1. shall not be elemental" }
    procedure(x) :: z
  end subroutine sub1
  subroutine sub2(z) ! { dg-error "Procedure pointer 'z' at .1. shall not be elemental" }
    procedure(x), pointer :: z
  end subroutine sub2
  subroutine sub3(z)
    interface
      elemental real function z(y) ! { dg-error "Dummy procedure 'z' at .1. shall not be elemental" }
        real, intent(in) :: y
      end function z
    end interface
  end subroutine sub3
  subroutine sub4(z)
    interface
      elemental real function z(y) ! { dg-error "Procedure pointer 'z' at .1. shall not be elemental" }
        real, intent(in) :: y
      end function z
    end interface
    pointer :: z
  end subroutine sub4
  subroutine sub5(z) ! { dg-error "Dummy procedure 'z' at .1. shall not be elemental" }
    procedure(sin) :: z
  end subroutine sub5
end
