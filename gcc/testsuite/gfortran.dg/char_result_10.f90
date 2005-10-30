! { dg-do compile }
! PR 18883: Fake result variables of non-constant length, with ENTRY
function s_to_c(chars)
    character, pointer :: chars(:)
    character(len=len(chars)) :: s_to_c, s_to_c_2
    s_to_c = 'a'
    return
entry s_to_c_2(chars)
    s_to_c_2 = 'b'
    return
end function s_to_c

program huj

    implicit none
    interface
        function s_to_c(chars)
            character, pointer :: chars(:)
            character(len=len(chars)) :: s_to_c
        end function s_to_c

        function s_to_c_2(chars)
            character, pointer :: chars(:)
            character(len=len(chars)) :: s_to_c_2
        end function s_to_c_2
    end interface

    character, pointer :: c(:)
    character(3) :: s

    allocate(c(5))
    c = (/"a", "b", "c" /)
    s = s_to_c(c)
    s = s_to_c_2(c)

end program huj
