! { dg-do run }
! { dg-options "-finit-integer=1 -finit-logical=true -finit-real=zero" }

program init_flag_2
  call real_test
  call logical_test
  call int_test
  call complex_test
end program init_flag_2

! Test some initializations for both implicitly and
! explicitly declared local variables.
subroutine real_test
  real r1
  real r2(10)
  dimension r3(10,10)
  if (r1 /= 0.0) call abort
  if (r2(2) /= 0.0) call abort
  if (r3(5,5) /= 0.0) call abort
  if (r4 /= 0.0) call abort
end subroutine real_test

subroutine logical_test
  logical l1
  logical l2(2)
  if (l1 .neqv. .true.) call abort
  if (l2(2) .neqv. .true.) call abort
end subroutine logical_test

subroutine int_test
  integer i1
  integer i2(10)
  dimension i3(10,10)
  if (i1 /= 1) call abort
  if (i2(2) /= 1) call abort
  if (i3(5,5) /= 1) call abort
  if (i4 /= 1) call abort
end subroutine int_test

subroutine complex_test
  complex c1
  complex c2(20,20)
  if (c1 /= (0.0,0.0)) call abort
  if (c2(1,1) /= (0.0,0.0)) call abort 
end subroutine complex_test
