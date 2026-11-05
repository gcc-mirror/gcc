! { dg-additional-options "-fdump-tree-original" }
subroutine foo
  implicit none

  !$omp teams dyn_groupprivate(fallback(abort) : 123)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
  !$omp end teams

  !$omp target
    !$omp teams dyn_groupprivate(fallback(null) : 234)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
    !$omp end teams
  !$omp end target

  !$omp teams dyn_groupprivate(fallback(default_mem) : 345)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
  !$omp end teams

  !$omp target teams  dyn_groupprivate(cgroup: 567)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
  !$omp end target teams
end

! { dg-final { scan-tree-dump-times "#pragma omp teams dyn_groupprivate\\(fallback\\(abort\\):123\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target dyn_groupprivate\\(fallback\\(null\\):234\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams dyn_groupprivate\\(fallback\\(default_mem\\):345\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target dyn_groupprivate\\(567\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams\[\\n\\r\]" 2 "original" } }
