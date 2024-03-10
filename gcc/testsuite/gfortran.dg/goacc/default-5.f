! OpenACC default (present) clause.

! { dg-additional-options "-fdump-tree-gimple" } 

      SUBROUTINE F1
      IMPLICIT NONE
      INTEGER :: F1_A = 2, F1_C = 3
      REAL, DIMENSION (2) :: F1_B, F1_D

!$ACC KERNELS DEFAULT (PRESENT)
! { dg-final { scan-tree-dump-times "omp target oacc_kernels default\\(present\\) map\\(force_present:f1_b \[^\\)\]+\\) map\\(force_tofrom:f1_a" 1 "gimple" } }
      F1_B(1) = F1_A;
!$ACC END KERNELS
!$ACC PARALLEL DEFAULT (PRESENT)
! { dg-final { scan-tree-dump-times "omp target oacc_parallel default\\(present\\) map\\(force_present:f1_b \[^\\)\]+\\) firstprivate\\(f1_a\\)" 1 "gimple" } }
      F1_B(1) = F1_A;
!$ACC END PARALLEL

!$ACC DATA DEFAULT (PRESENT)
!$ACC KERNELS
! { dg-final { scan-tree-dump-times "omp target oacc_kernels map\\(force_present:f1_d \[^\\)\]+\\) map\\(force_tofrom:f1_c" 1 "gimple" } }
      F1_D(1) = F1_C;
!$ACC END KERNELS
!$ACC END DATA
!$ACC DATA DEFAULT (NONE)
!$ACC DATA DEFAULT (PRESENT)
!$ACC PARALLEL DEFAULT (PRESENT)
! { dg-final { scan-tree-dump-times "omp target oacc_parallel default\\(present\\) map\\(force_present:f1_d \[^\\)\]+\\) firstprivate\\(f1_c\\)" 1 "gimple" } }
      F1_D(1) = F1_C;
!$ACC END PARALLEL
!$ACC END DATA
!$ACC END DATA
      END SUBROUTINE F1
