! { dg-do compile }
! PR 18883: Fake result variables of non-constant length, in module
module foo
contains
    function s_to_c(chars)
        character, pointer :: chars(:)
        character(len=len(chars)) :: s_to_c
        s_to_c = 'a'
    end function s_to_c
end module foo

program huj

    use foo

    implicit none
    character, pointer :: c(:)
    character(3) :: s

    allocate(c(5))
    c = (/"a", "b", "c" /)
    s = s_to_c(c)

end program huj

! { dg-final { cleanup-modules "foo" } }
