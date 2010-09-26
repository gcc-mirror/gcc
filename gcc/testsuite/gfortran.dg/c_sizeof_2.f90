! { dg-do compile }
! { dg-options "-std=f2003 -Wall -Wno-conversion" }
! Support F2008's c_sizeof()
!
USE ISO_C_BINDING, only: C_SIZE_T, c_sizeof ! { dg-error "new in Fortran 2008" }
integer(C_SIZE_T) :: i
i = c_sizeof(i)           
end

