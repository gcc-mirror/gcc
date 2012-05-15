! { dg-do run }
!
! PR fortran/43199
!
! This program gave an ICE due to reading the REF_COMPONENT with CLASS.
!
module m_string
  type t_string
      character, dimension(:), allocatable :: string
  end type t_string
contains
pure function string_to_char ( s ) result(res)
  class(t_string), intent(in) :: s
  character(len=size(s%string)) :: res
  integer :: i
  do i = 1,len(res)
    res(i:i) = s%string(i)
  end do
end function string_to_char
end module m_string

use m_string
type(t_string) :: str
allocate(str%string(5))
str%string = ['H','e','l','l','o']
if (len (string_to_char (str)) /= 5) call abort ()
if (string_to_char (str) /= "Hello") call abort ()
end
