! { dg-do compile }
!
! PR fortran/44346
! Original test sumbitted by Vittorio Zecca, zeccav at gmail dot com.
! Modified by Steven G. Kargl for dejagnu testsuite.
!
program a
   integer :: n = 42
   ! 64 + 3 > bitsize(n) 
   call mvbits(n, 64, 3, n, 1)   ! { dg-error "must be less than" }
   ! 64 + 2 > bitsize(n)        
   call mvbits(n, 30, 2, n, 64)  ! { dg-error "must be less than" }
   ! LEN negative
   call mvbits(n, 30, -2, n, 30) ! { dg-error "must be nonnegative" }
   ! TOPOS negative
   call mvbits(n, 30, 2, n, -3)  ! { dg-error "must be nonnegative" }
   ! FROMPOS negative
   call mvbits(n, -1, 2, n, 3)   ! { dg-error "must be nonnegative" }
end program a
