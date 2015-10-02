! { dg-do compile }
! PR 62242
! A subprogram calling an array constructor with an array element whose
! value is the result of calling a character function with both an
! assumed-length argument and an assumed-length result
module gfbug
    implicit none
contains
    function inner(inner_str) result(upper)
        character(*), intent(IN) :: inner_str
        character(LEN(inner_str)) :: upper

        upper = '123'
    end function

    subroutine outer(outer_str)
        character(*), intent(IN) :: outer_str
        character(5) :: z(1)

        z = [inner(outer_str)]
    end subroutine
end module gfbug
