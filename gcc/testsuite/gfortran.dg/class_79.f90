!{ dg-do run }

! Check double free on array constructor in argument list is fixed.
! Contributed by Damian Rouson  <damian@archaeologic.codes>
program pr119349
  implicit none
  
  type string_t
    character(len=:), allocatable :: string_
  end type

  print *, true([string()])

contains

  type(string_t) function string()
    string%string_ = ""
  end function

  logical elemental function true(rhs)
    class(string_t), intent(in) :: rhs
    true = .true.
  end function

end program
