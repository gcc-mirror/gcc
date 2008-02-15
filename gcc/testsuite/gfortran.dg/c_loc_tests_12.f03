! { dg-do compile }
! 
! Test for PR 35150, reduced testcases by Tobias Burnus
!
module test1
  use, intrinsic :: iso_c_binding
  implicit none
contains
  subroutine sub1(argv) bind(c,name="sub1")
    type(c_ptr), intent(in) :: argv
  end subroutine

  subroutine sub2
    type(c_ptr), dimension(1), target :: argv = c_null_ptr
    character(c_char), dimension(1), target :: s = c_null_char
    call sub1(c_loc(argv))
  end subroutine
end module test1

program test2
  use iso_c_binding
  type(c_ptr), target, save :: argv
  interface
    subroutine sub1(argv) bind(c)
      import
      type(c_ptr) :: argv
    end subroutine sub1
  end interface
  call sub1(c_loc(argv))
end program test2
!
! { dg-final { cleanup-modules "test1" } }
