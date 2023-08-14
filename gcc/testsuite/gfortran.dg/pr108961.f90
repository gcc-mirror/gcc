! { dg-do run }
!
! Contributed by Jeffrey Hill  <jeffrey.p.hill@nasa.gov>
!
module associate_ptr
    use iso_c_binding
contains
    subroutine c_f_strpointer(cptr, ptr2)
        type(c_ptr), target, intent(in) :: cptr
        character(kind=c_char,len=4), pointer :: ptr1
        character(kind=c_char,len=:), pointer, intent(out) :: ptr2
        call c_f_pointer(cptr, ptr1)
        if (ptr1 .ne. 'abcd') stop 1
        ptr2 => ptr1                              ! Failed here
    end subroutine
end module

program test_associate_ptr
    use associate_ptr
    character(kind=c_char, len=1), target :: char_array(7)
    character(kind=c_char,len=:), pointer :: ptr2
    char_array = ['a', 'b', 'c', 'd', c_null_char, 'e', 'f']
! The first argument was providing a constant hidden string length => segfault
    call c_f_strpointer(c_loc(char_array), ptr2)
    if (ptr2 .ne. 'abcd') stop 2
end program
