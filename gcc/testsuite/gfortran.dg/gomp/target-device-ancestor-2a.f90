! { dg-do compile }

implicit none

integer :: a, b, c

!$omp requires reverse_offload

!$omp target device (ancestor: 1)
!$omp end target

!$omp target device (ancestor : a)
!$omp end target

!$omp target device (ancestor : a + 1)
!$omp end target


!$omp target device (device_num:42)
!$omp end target

!$omp target device (42)
!$omp end target


! Ensure that no OpenMP constructs appear inside target regions with 'ancestor'.

!$omp target device (ancestor: 1)
  !$omp teams  ! { dg-error "OpenMP constructs are not allowed in target region with 'ancestor'" }
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

!$omp target nowait device (ancestor: 1)  ! { dg-error "with 'ancestor', only the 'device', 'firstprivate', 'private', 'defaultmap', and 'map' clauses may appear on the construct" }
!$omp end target

!$omp target device (ancestor: 1) nowait  ! { dg-error "with 'ancestor', only the 'device', 'firstprivate', 'private', 'defaultmap', and 'map' clauses may appear on the construct" }
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

!$omp target data map (a) device (ancestor: 1)  ! { dg-error "'device' clause with 'ancestor' is only allowed on 'target' construct" }
!$omp end target data

!$omp target enter data map (to: a) device (ancestor: 1)  ! { dg-error "'device' clause with 'ancestor' is only allowed on 'target' construct" }
!$omp target exit data map (from: a) device (ancestor: 1)  ! { dg-error "'device' clause with 'ancestor' is only allowed on 'target' construct" }

!$omp target update to (a) device (ancestor: 1)  ! { dg-error "'device' clause with 'ancestor' is only allowed on 'target' construct" }

!$omp target device (ancestor: 1) if(.false.)
! { dg-error "with 'ancestor', only the 'device', 'firstprivate', 'private', 'defaultmap', and 'map' clauses may appear on the construct" "" { target *-*-* } .-1 }
!$omp end target

end
