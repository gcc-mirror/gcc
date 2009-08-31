! { dg-do run }
! { dg-options "-finit-integer=-1 -finit-logical=false -finit-real=nan" }
! { dg-add-options ieee }
! { dg-skip-if "NaN not supported" { spu-*-* } { "*" } { "" } }

program init_flag_3
  call real_test
  call logical_test
  call int_test
  call complex_test
end program init_flag_3

! Test some initializations for both implicitly and
! explicitly declared local variables.
subroutine real_test
  real r1
  real r2(10)
  dimension r3(10,10)
  if (r1 .eq. r1) call abort
  if (r2(2) .eq. r2(2)) call abort
  if (r3(5,5) .eq. r3(5,5)) call abort
  if (r4 .eq. r4) call abort
end subroutine real_test

subroutine logical_test
  logical l1
  logical l2(2)
  if (l1 .neqv. .false.) call abort
  if (l2(2) .neqv. .false.) call abort
end subroutine logical_test

subroutine int_test
  integer i1
  integer i2(10)
  dimension i3(10,10)
  if (i1 /= -1) call abort
  if (i2(2) /= -1) call abort
  if (i3(5,5) /= -1) call abort
  if (i4 /= -1) call abort
end subroutine int_test

subroutine complex_test
  complex c1
  complex c2(20,20)
  if (c1 .eq. c1) call abort
  if (c2(1,1) .eq. c2(1,1)) call abort 
end subroutine complex_test
