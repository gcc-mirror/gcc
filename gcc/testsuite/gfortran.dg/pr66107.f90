! { dg-do compile }
! PR fortran/66107
subroutine p
 integer n
 character(*), parameter :: z(1) = [character(len=n) :: 'x'] ! { dg-error "Cannot initialize parameter array at .1. with variable length elements" }
end subroutine
