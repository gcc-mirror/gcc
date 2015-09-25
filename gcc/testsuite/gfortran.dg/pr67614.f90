! { dg-do compile }
! { dg-options "-std=legacy" }
! PR fortran/67614
!
program foo
   implicit none
   integer, pointer :: z
   if (null(z)) 10, 20, 30    ! { dg-error "Invalid NULL" }
10 continue
20 continue
30 continue
end program foo
