! PR fortran/77371
! { dg-do compile }
program p
   integer, allocatable :: n
!$acc parallel reduction (+:n) private(n) ! { dg-error "invalid private reduction" }
!$acc end parallel
end
