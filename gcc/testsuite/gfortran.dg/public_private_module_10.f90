! { dg-do compile }
! PR 87734 - this used to issue spurious errors.

module m_vstring
  implicit none

  public :: vstring_length

contains

  subroutine vstring_cast()
    character ( len = vstring_length() ) :: char_string
  end subroutine

  pure integer function vstring_length ()
  end function

end module
