! { dg-do compile }
! { dg-options "-Wall" }
! PR fortran/27553
program pr27553
10: a=10   ! { dg-error "character in statement" }
end program
