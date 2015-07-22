! { dg-do compile }
!
! PR fortran/66929
! Generic procedures as actual argument used to lead to
! a NULL pointer dereference in gfc_get_proc_ifc_for_expr
! because the generic symbol was used as procedure symbol,
! instead of the specific one.

module iso_varying_string
  type, public :: varying_string
     character(LEN=1), dimension(:), allocatable :: chars
  end type varying_string
  interface operator(/=)
     module procedure op_ne_VS_CH
  end interface operator (/=)
  interface trim
     module procedure trim_
  end interface
contains
  elemental function op_ne_VS_CH (string_a, string_b) result (op_ne)
    type(varying_string), intent(in) :: string_a
    character(LEN=*), intent(in)     :: string_b
    logical                          :: op_ne
    op_ne = .true.
  end function op_ne_VS_CH
  elemental function trim_ (string) result (trim_string)
    type(varying_string), intent(in) :: string
    type(varying_string)             :: trim_string
    trim_string = varying_string(["t", "r", "i", "m", "m", "e", "d"])
  end function trim_
end module iso_varying_string
module syntax_rules
  use iso_varying_string, string_t => varying_string
contains
  subroutine set_rule_type_and_key
    type(string_t) :: key
    if (trim (key) /= "") then
      print *, "non-empty"
    end if
  end subroutine set_rule_type_and_key
end module syntax_rules
