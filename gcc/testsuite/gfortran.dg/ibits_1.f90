! { dg-do compile }
!
! PR fortran/44346
! Original test sumbitted by Vittorio Zecca, zeccav at gmail dot com.
! Modified by Steven G. Kargl for dejagnu testsuite.
!
program a
   integer :: j, i = 42
   j = ibits(i, -1, 1)    ! { dg-error "must be nonnegative" }
   j = ibits(i, 1, -1)    ! { dg-error "must be nonnegative" }
   j = ibits(i, 100, 100) ! { dg-error "must be less than" }
end program a

