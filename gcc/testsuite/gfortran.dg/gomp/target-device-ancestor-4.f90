! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

! Test to ensure that device-modifier 'ancestor' is parsed correctly in
! device clauses.

!$omp requires reverse_offload  ! { dg-error "Sorry, 'reverse_offload' clause at \\(1\\) on REQUIRES directive is not yet supported" }

!$omp target device (ancestor : 1)  ! { dg-message "" "sorry, unimplemented: 'ancestor' not yet supported" { xfail *-*-* } }
!$omp end target

end

! TODO: dg-final { scan-tree-dump-times "pragma omp target \[^\n\r)]*device\\(ancestor:1\\)" 1 "original" } }
