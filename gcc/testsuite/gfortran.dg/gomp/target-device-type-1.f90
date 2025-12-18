! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

!$omp target
!$omp end target

!$omp target device_type ( any )
!$omp end target

!$omp target device_type ( nohost )  ! { dg-message "sorry, unimplemented: only the 'device_type\\(any\\)' is supported" }
!$omp end target

!$omp target device_type ( host )
!$omp end target

end

! { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(-2\\) thread_limit\\(0\\)\[\\r\\n\]" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(-2\\) thread_limit\\(0\\) device_type\\(any\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(-2\\) thread_limit\\(0\\) device_type\\(nohost\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "#pragma omp target num_teams\\(-2\\) thread_limit\\(0\\) device_type\\(host\\)" 1 "gimple" } }
