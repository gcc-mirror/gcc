! { dg-do run }
! Checks Fortran 2003's new_line intrinsic function
! PR fortran/28585
program new_line_check
    implicit none
    if(achar(10) /= new_line('a')) call abort()
end program new_line_check
