! { dg-do compile }
! { dg-options -Wexternal-argument-mismatch }
! PR fortran/120163 - this used to cause an error.
! Original test case by Bálint Aradi
module mod1
  implicit none

  abstract interface
    pure subroutine callback_interface(a)
      real, intent(in) :: a
    end subroutine callback_interface
  end interface

contains

  subroutine caller(callback)
    procedure(callback_interface) :: callback
    real :: a
    call callback(a)
  end subroutine caller

end module mod1


module mod2
  use mod1
end module mod2
