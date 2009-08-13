! { dg-do compile }
! PR 33229
implicit none
intrinsic cpu_time  ! { dg-error "attribute conflicts with" }
real :: time
print *, CPU_TIME(TIME)  ! { dg-error "is not a function" }
end
