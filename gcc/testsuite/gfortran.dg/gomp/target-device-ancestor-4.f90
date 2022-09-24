! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

! Test to ensure that device-modifier 'ancestor' is parsed correctly in
! device clauses.

!$omp requires reverse_offload

!$omp target device (ancestor : 1)
!$omp end target

end

! { dg-final { scan-tree-dump-times "pragma omp target \[^\n\r)]*device\\(ancestor:1\\)" 1 "original" } }
