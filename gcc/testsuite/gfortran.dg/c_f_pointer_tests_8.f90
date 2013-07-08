! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR fortran/57834
!
! (Gave a bogus warning before.)
!
program main

    use iso_c_binding
    use iso_fortran_env

    implicit none

    interface
        function strerror(errno) bind(C, NAME = 'strerror')
            import
            type(C_PTR) :: strerror
            integer(C_INT), value :: errno
        end function
    end interface

    integer :: i
    type(C_PTR) :: cptr
    character(KIND=C_CHAR), pointer :: str(:)

    cptr = strerror(INT(42, KIND = C_INT))
    call C_F_POINTER(cptr, str, [255])

    do i = 1, SIZE(str)
        if (str(i) == C_NULL_CHAR) exit
        write (ERROR_UNIT, '(A1)', ADVANCE = 'NO') str(i:i)
    enddo

    write (ERROR_UNIT, '(1X)')

end program main
