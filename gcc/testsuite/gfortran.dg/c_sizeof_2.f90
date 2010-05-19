! { dg-do compile }
! { dg-options "-std=f2003 -Wall -Wno-conversion" }
! Support F2008's c_sizeof()
!
USE ISO_C_BINDING
integer(C_SIZE_T) :: i
i = c_sizeof(i)           ! { dg-warning "Fortran 2008" }
end

