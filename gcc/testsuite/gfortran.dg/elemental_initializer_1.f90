! { dg-do compile }
! Tests the fix for elemental functions not being allowed in
! specification expressions in pure procedures.
!
! Testcase from iso_varying_string by Rich Townsend <rhdt@star.ucl.ac.uk>
! The allocatable component has been changed to a pointer for this testcase.
!
module iso_varying_string

  type varying_string
     private
     character(LEN=1), dimension(:), pointer :: chars
  end type varying_string

  interface len
     module procedure len_
  end interface len

contains

  pure function char_auto (string) result (char_string)
    type(varying_string), intent(in) :: string
    character(LEN=len(string))       :: char_string ! Error was here
    char_string = ""
  end function char_auto

  elemental function len_ (string) result (length)
    type(varying_string), intent(in) :: string
    integer                          :: length
    length = 1
  end function len_

end module iso_varying_string

! { dg-final { cleanup-modules "iso_varying_string" } }
