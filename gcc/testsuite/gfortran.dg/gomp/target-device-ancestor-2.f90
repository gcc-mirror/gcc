! { dg-do compile }

implicit none

integer :: a, b, c

!$omp requires reverse_offload  ! { dg-error "Sorry, 'reverse_offload' clause at \\(1\\) on REQUIRES directive is not yet supported" }


! The following test case is marked with 'xfail' because a previous 'sorry' from
! 'reverse_offload' suppresses the 'sorry' for 'ancestor'.

!$omp target device (ancestor: 1)  ! { dg-message "" "sorry, unimplemented: 'ancestor' not yet supported" { xfail *-*-* } }
!$omp end target

!$omp target device (ancestor : a)  ! { dg-message "" "sorry, unimplemented: 'ancestor' not yet supported" { xfail *-*-* } }
!$omp end target

!$omp target device (ancestor : a + 1)  ! { dg-message "" "sorry, unimplemented: 'ancestor' not yet supported" { xfail *-*-* } }
!$omp end target


! Ensure that the integer expression in the 'device' clause for
! device-modifier 'ancestor' evaluates to '1' in case of a constant.

!$omp target device (ancestor: 42)  ! { dg-error "the 'device' clause expression must evaluate to '1'" }
! !$omp end target

!$omp target device (device_num:42)
!$omp end target

!$omp target device (42)
!$omp end target


! Ensure that no OpenMP constructs appear inside target regions with 'ancestor'.
! The following test case is marked with 'xfail' because a previous 'sorry' from
! 'reverse_offload' suppresses the 'sorry' for 'ancestor'.

!$omp target device (ancestor: 1)
  !$omp teams  ! { dg-error "" "OpenMP constructs are not allowed in target region with 'ancestor'" { xfail *-*-* } }
  !$omp end teams
!$omp end target

!$omp target device (device_num: 1)
  !$omp teams
  !$omp end teams
!$omp end target

!$omp target device (1)
  !$omp teams
  !$omp end teams
!$omp end target


! Ensure that with 'ancestor' only the 'device', 'firstprivate', 'private',
! 'defaultmap', and 'map' clauses appear on the construct.
! The following test case is marked with 'xfail' because a previous 'sorry' from
! 'reverse_offload' suppresses the 'sorry' for 'ancestor'.

!$omp target nowait device (ancestor: 1)  ! { dg-error "" "with 'ancestor', only the 'device', 'firstprivate', 'private', 'defaultmap', and 'map' clauses may appear on the construct" { xfail *-*-* } }
!$omp end target

!$omp target device (ancestor: 1) nowait  ! { dg-error "" "with 'ancestor', only the 'device', 'firstprivate', 'private', 'defaultmap', and 'map' clauses may appear on the construct" { xfail *-*-* } }
!$omp end target

!$omp target nowait device (device_num: 1)
!$omp end target

!$omp target nowait device (1)
!$omp end target

!$omp target device (ancestor: 1) firstprivate (a) private (b) defaultmap (none) map (c)
!$omp end target


! Ensure that 'ancestor' is only used with 'target' constructs (not with
! 'target data', 'target update' etc.).
! The following test case is marked with 'xfail' because a previous 'sorry' from
! 'reverse_offload' suppresses the 'sorry' for 'ancestor'.

!$omp target data map (a) device (ancestor: 1)  ! { dg-error "" "'device' clause with 'ancestor' is only allowed on 'target' construct" { xfail *-*-* } }
!$omp end target data

!$omp target enter data map (to: a) device (ancestor: 1)  ! { dg-error "" "'device' clause with 'ancestor' is only allowed on 'target' construct" { xfail *-*-* } }
!$omp target exit data map (from: a) device (ancestor: 1)  ! { dg-error "" "'device' clause with 'ancestor' is only allowed on 'target' construct" { xfail *-*-* } }

!$omp target update to (a) device (ancestor: 1)  ! { dg-error "'device' clause with 'ancestor' is only allowed on 'target' construct" "" { xfail *-*-* } }
! { dg-error "with 'ancestor', only the 'device', 'firstprivate', 'private', 'defaultmap', and 'map' clauses may appear on the construct" "" { xfail *-*-* } .-1 }


end