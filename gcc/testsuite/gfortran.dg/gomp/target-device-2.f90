! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

! Test to ensure that device-modifier 'device_num' is parsed correctly in
! device clauses.

!$omp target device (device_num : 42)
!$omp end target

end

! { dg-final { scan-tree-dump "pragma omp target \[^\n\r)]*device\\(42\\)" "original" } }
