! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

implicit none

integer :: N, M
N = 1024
M = 128

!$omp target dyn_groupprivate(1024)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
!$omp end target

!$omp target dyn_groupprivate (1024 * N)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
!$omp end target

!$omp target dyn_groupprivate ( fallback ( abort ) : N)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
!$omp end target

!$omp target dyn_groupprivate ( fallback ( null ) : N)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
!$omp end target

!$omp target dyn_groupprivate ( fallback ( default_mem ) : N)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
!$omp end target

!$omp target dyn_groupprivate(cgroup : 4096)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
!$omp end target

!$omp target dyn_groupprivate ( fallback ( abort ) , cgroup : M)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
!$omp end target

!$omp target dyn_groupprivate ( cgroup, fallback ( null ) : M)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
!$omp end target

!$omp target dyn_groupprivate ( fallback ( default_mem ),cgroup : M)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
!$omp end target

end

! { dg-final { scan-tree-dump-times "#pragma omp target dyn_groupprivate\\(1024\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target dyn_groupprivate\\(D\\.\[0-9\]+\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target dyn_groupprivate\\(fallback\\(abort\\):n\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target dyn_groupprivate\\(fallback\\(null\\):n\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target dyn_groupprivate\\(fallback\\(default_mem\\):n\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target dyn_groupprivate\\(4096\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target dyn_groupprivate\\(fallback\\(abort\\):m\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target dyn_groupprivate\\(fallback\\(null\\):m\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target dyn_groupprivate\\(fallback\\(default_mem\\):m\\)" 1 "original" } }
