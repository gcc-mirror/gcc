! { dg-do compile }
!
! PR 40743: [4.5 Regression] ICE when compiling iso_varying_string.f95 at revision 149591
!
! Reduced from http://www.fortran.com/iso_varying_string.f95
! Contributed by Janus Weil <janus@gcc.gnu.org>

  implicit none

  type :: varying_string
  end type

  interface assignment(=)
     procedure op_assign_VS_CH
  end interface

contains

  subroutine op_assign_VS_CH (var, exp) ! { dg-warning "Extension: Internal procedure" }
    type(varying_string), intent(out) :: var
    character(LEN=*), intent(in)      :: exp
  end subroutine

  subroutine split_VS
    type(varying_string) :: string
    call split_CH(string)
  end subroutine

  subroutine split_CH (string)
    type(varying_string) :: string
    string = ""
  end subroutine

end

