! { dg-do compile }
!
! PR fortran/49110
! PR fortran/52843
!
! Based on a contributed code by jwmwalrus@gmail.com
!
! Before, character(len=:) result variable were rejected in PURE functions. 
!
module mod1
    use iso_c_binding
    implicit none

contains
    pure function c_strlen(str)
      character(KIND = C_CHAR), intent(IN) :: str(*)
      integer :: c_strlen,i

      i = 1
      do
        if (i < 1) then
          c_strlen = 0
          return
        end if
        if (str(i) == c_null_char) exit
        i = i + 1
      end do
      c_strlen = i - 1
    end function c_strlen
    pure function c2fstring(cbuffer) result(string)
        character(:), allocatable :: string
        character(KIND = C_CHAR), intent(IN) :: cbuffer(*)
        integer :: i

    continue
        string = REPEAT(' ', c_strlen(cbuffer))

        do i = 1, c_strlen(cbuffer)
            if (cbuffer(i) == C_NULL_CHAR) exit
            string(i:i) = cbuffer(i)
        enddo

        string = TRIM(string)
    end function
end module mod1

use mod1
character(len=:), allocatable :: str
str = c2fstring("ABCDEF"//c_null_char//"GHI")
if (len(str) /= 6 .or. str /= "ABCDEF") STOP 1
end
