! { dg-do compile }
! Test the fix for PR43227, in which the lines below would segfault.
!
! Dominique d'Humieres <dominiq@lps.ens.fr>
!
function char1 (s) result(res)
  character, dimension(:), intent(in) :: s
  character(len=size(s)) :: res
  do i = 1, size(s)
    res(i:i) = s(i)
  end do
end function char1

module m_string

  procedure(string_to_char) :: char1                    ! segfault
  procedure(string_to_char), pointer :: char2           ! segfault
  type t_string
    procedure(string_to_char), pointer, nopass :: char3 ! segfault
  end type t_string

contains

  function string_to_char (s) result(res)
    character, dimension(:), intent(in) :: s
    character(len=size(s)) :: res
    do i = 1, size(s)
      res(i:i) = s(i)
    end do
  end function string_to_char

end module m_string

  use m_string
  type(t_string) :: t
  print *, string_to_char (["a","b","c"])
  char2 => string_to_char
  print *, char2 (["d","e","f"])
  t%char3 => string_to_char
  print *, t%char3 (["g","h","i"])
  print *, char1 (["j","k","l"])
end
