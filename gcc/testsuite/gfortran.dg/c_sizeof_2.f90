! { dg-do compile }
! { dg-options "-std=f2003 -Wall" }
! Support F2008's c_sizeof()
!
integer(4) :: i
i = c_sizeof(i) ! { dg-warning "Fortran 2008" }
end

