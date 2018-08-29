! Test valid usage and processing of the finalize clause.

! { dg-additional-options "-fdump-tree-original -fdump-tree-gimple" }

      SUBROUTINE f
      IMPLICIT NONE
      INTEGER :: del_r
      REAL, DIMENSION (3) :: del_f
      DOUBLE PRECISION, DIMENSION (8) :: cpo_r
      LOGICAL :: cpo_f

!$ACC EXIT DATA DELETE (del_r)
! { dg-final { scan-tree-dump-times "(?n)#pragma acc exit data map\\(release:del_r\\);$" 1 "original" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_enter_exit_data map\\(release:del_r \\\[len: \[0-9\]+\\\]\\)$" 1 "gimple" } }

!$ACC EXIT DATA FINALIZE DELETE (del_f)
! { dg-final { scan-tree-dump-times "(?n)#pragma acc exit data map\\(release:del_f\\) finalize;$" 1 "original" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_enter_exit_data map\\(delete:del_f \\\[len: \[0-9\]+\\\]\\) finalize$" 1 "gimple" } }

!$ACC EXIT DATA COPYOUT (cpo_r)
! { dg-final { scan-tree-dump-times "(?n)#pragma acc exit data map\\(from:cpo_r\\);$" 1 "original" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_enter_exit_data map\\(from:cpo_r \\\[len: \[0-9\]+\\\]\\)$" 1 "gimple" } }

!$ACC EXIT DATA COPYOUT (cpo_f) FINALIZE
! { dg-final { scan-tree-dump-times "(?n)#pragma acc exit data map\\(from:cpo_f\\) finalize;$" 1 "original" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_enter_exit_data map\\(force_from:cpo_f \\\[len: \[0-9\]+\\\]\\) finalize$" 1 "gimple" } }
      END SUBROUTINE f
