! { dg-do link }
!
! PR fortran/40568
!
! Module checks for C_SIZEOF (part of ISO_C_BINDING)
!
subroutine test
use iso_c_binding, only: foo => c_sizeof, bar=> c_sizeof, c_sizeof, c_int
integer(c_int) :: i
print *, c_sizeof(i), bar(i), foo(i)
end

use iso_c_binding
implicit none
integer(c_int) :: i
print *, c_sizeof(i)
call test()
end
