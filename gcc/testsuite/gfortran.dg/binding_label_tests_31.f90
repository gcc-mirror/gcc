! { dg-do compile }
! PR fortran/66695 - this used to ICE.
! Original test case by Vladimir Fuka.
module mod
  implicit none
contains
    integer function F()
    end function
end module
    
module mod_C
  use mod
  implicit none
contains
  subroutine s()  bind(C, name="f")
    integer :: x
      x = F()
  end subroutine
end module
