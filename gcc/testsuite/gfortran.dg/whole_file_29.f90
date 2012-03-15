! { dg-do compile }
! Test the fix for the problem described in PR45077 comments #4 and #5.
! Note that the module file from whole_file_28.f90, 'iso_red', is
! needed for this test.
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
module ifiles
  use iso_red, string_t => varying_string
contains
  function line_get_string_advance (line) result (string)
    type(string_t) :: string
    character :: line
  end function line_get_string_advance
end module ifiles

module syntax_rules
  use iso_red, string_t => varying_string
  use ifiles, only: line_get_string_advance
contains
  subroutine syntax_init_from_ifile ()
    type(string_t) :: string
       string = line_get_string_advance ("")
  end subroutine syntax_init_from_ifile
end module syntax_rules
end
! { dg-final { cleanup-modules "iso_red" } }
