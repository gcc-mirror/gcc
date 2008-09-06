! { dg-do compile }
! PR 33229
implicit none
intrinsic cpu_time
real :: time
print *, CPU_TIME(TIME)  ! { dg-error "Intrinsic subroutine" }
end
