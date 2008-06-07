! { dg-do compile }
! { dg-options "-std=f2003 -Wall" }
! Support F2008's c_sizeof()
!
integer(4) :: i, j(10)
i = c_sizeof(i) ! { dg-error "not included in the selected standard" }
i = c_sizeof(j) ! { dg-error "not included in the selected standard" }
end

