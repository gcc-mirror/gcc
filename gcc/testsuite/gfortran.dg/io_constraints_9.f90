! { dg-do compile }
!
! PR fortran/48972
!
! All string arguments to I/O statements shall
! be of default-character type. (Except for the
! internal unit.)
!
character(len=20, kind=4) :: str1

write(99, str1) 'a'  ! { dg-error "must be of type default-kind CHARACTER" }
read(99, fmt=str1)   ! { dg-error "must be of type default-kind CHARACTER" }
end
