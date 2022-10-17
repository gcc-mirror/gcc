! Test valid usage and processing of the finalize clause.

! { dg-additional-options "-fdump-tree-original -fdump-tree-gimple" }

      SUBROUTINE f
      IMPLICIT NONE
      INTEGER :: del_r
      REAL, DIMENSION (3) :: del_f
      INTEGER (1), DIMENSION (:), ALLOCATABLE :: del_f_p
      DOUBLE PRECISION, DIMENSION (8) :: cpo_r
      LOGICAL :: cpo_f
      INTEGER (1), DIMENSION (:), ALLOCATABLE :: cpo_f_p

!$ACC EXIT DATA DELETE (del_r)
! { dg-final { scan-tree-dump-times "(?n)#pragma acc exit data map\\(release:del_r\\);$" 1 "original" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_exit_data map\\(release:del_r \\\[len: \[0-9\]+\\\]\\)$" 1 "gimple" } }

!$ACC EXIT DATA FINALIZE DELETE (del_f)
! { dg-final { scan-tree-dump-times "(?n)#pragma acc exit data map\\(release:del_f\\) finalize;$" 1 "original" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_exit_data map\\(delete:del_f \\\[len: \[0-9\]+\\\]\\) finalize$" 1 "gimple" } }

!$ACC EXIT DATA FINALIZE DELETE (del_f_p(2:5))
! { dg-final { scan-tree-dump-times "(?n)#pragma acc exit data map\\(release:\\*\\(integer\\(kind=.\\)\\\[0:\\\] \\*\\) parm\\.0\\.data \\\[len: \[^\\\]\]+\\\]\\) map\\(to:del_f_p \\\[pointer set, len: \[0-9\]+\\\]\\) map\\(alloc:\\(integer\\(kind=1\\)\\\[0:\\\] \\* restrict\\) del_f_p\\.data \\\[pointer assign, bias: \\(.*int.*\\) parm\\.0\\.data - \\(.*int.*\\) del_f_p\\.data\\\]\\) finalize;$" 1 "original" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_exit_data map\\(delete:MEM <\[^>\]+> \\\[\\(integer\\(kind=.\\)\\\[0:\\\] \\*\\)_\[0-9\]+\\\] \\\[len: \[^\\\]\]+\\\]\\) map\\(to:del_f_p \\\[pointer set, len: \[0-9\]+\\\]\\) map\\(alloc:del_f_p\\.data \\\[pointer assign, bias: \[^\\\]\]+\\\]\\) finalize$" 1 "gimple" } }

!$ACC EXIT DATA COPYOUT (cpo_r)
! { dg-final { scan-tree-dump-times "(?n)#pragma acc exit data map\\(from:cpo_r\\);$" 1 "original" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_exit_data map\\(from:cpo_r \\\[len: \[0-9\]+\\\]\\)$" 1 "gimple" } }

!$ACC EXIT DATA COPYOUT (cpo_f) FINALIZE
! { dg-final { scan-tree-dump-times "(?n)#pragma acc exit data map\\(from:cpo_f\\) finalize;$" 1 "original" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_exit_data map\\(force_from:cpo_f \\\[len: \[0-9\]+\\\]\\) finalize$" 1 "gimple" } }

!$ACC EXIT DATA COPYOUT (cpo_f_p(4:10)) FINALIZE
! { dg-final { scan-tree-dump-times "(?n)#pragma acc exit data map\\(from:\\*\\(integer\\(kind=.\\)\\\[0:\\\] \\*\\) parm\\.1\\.data \\\[len: \[^\\\]\]+\\\]\\) map\\(to:cpo_f_p \\\[pointer set, len: \[0-9\]+\\\]\\) map\\(alloc:\\(integer\\(kind=1\\)\\\[0:\\\] \\* restrict\\) cpo_f_p\\.data \\\[pointer assign, bias: \\(.*int.*\\) parm\\.1\\.data - \\(.*int.*\\) cpo_f_p\\.data\\\]\\) finalize;$" 1 "original" } }
! { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_exit_data map\\(force_from:MEM <\[^>\]+> \\\[\\(integer\\(kind=.\\)\\\[0:\\\] \\*\\)_\[0-9\]+\\\] \\\[len: \[^\\\]\]+\\\]\\) map\\(to:cpo_f_p \\\[pointer set, len: \[0-9\]+\\\]\\) map\\(alloc:cpo_f_p\\.data \\\[pointer assign, bias: \[^\\\]\]+\\\]\\) finalize$" 1 "gimple" } }
      END SUBROUTINE f
