! { dg-do run }

! PR fortran/43829 
! Scalarization of reductions.
! Test that inlined sum is correct.

! We can't check for the absence of temporary arrays generated on the run-time
! testcase, as inlining is disabled at -Os, so it will fail in that case.
! Thus, the test is splitted into two independant files, one checking for
! the absence of temporaries, and one (this one) checking that the code
! generated remains valid at all optimization levels.
include 'inline_sum_1.f90'
