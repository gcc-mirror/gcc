! { dg-do compile }
!
! PR fortran/45077
!
! Contributed by Dominique d'Humieres, based on a test
! case of Juergen Reuter.
!

module iso_red
  type, public :: varying_string
     character(LEN=1), dimension(:), allocatable :: chars
  end type varying_string
end module iso_red

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

! { dg-final { cleanup-modules "iso_red ifiles syntax_rules" } }
