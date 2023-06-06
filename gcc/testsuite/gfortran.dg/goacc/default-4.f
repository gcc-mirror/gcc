! OpenACC default clause inside data construct.

! { dg-additional-options "-fdump-tree-gimple" } 

      SUBROUTINE F1
      IMPLICIT NONE
      INTEGER :: F1_A = 2
      REAL, DIMENSION (2) :: F1_B

!$ACC DATA COPYIN (F1_A) COPYOUT (F1_B)
! { dg-final { scan-tree-dump-times "omp target oacc_data map\\(to:f1_a \[^\\)\]+\\) map\\(from:f1_b" 1 "gimple" } }
!$ACC KERNELS
! { dg-final { scan-tree-dump-times "omp target oacc_kernels map\\(tofrom:f1_b \[^\\)\]+\\) map\\(tofrom:f1_a" 1 "gimple" } }
      F1_B(1) = F1_A;
!$ACC END KERNELS
!$ACC PARALLEL
! { dg-final { scan-tree-dump-times "omp target oacc_parallel map\\(tofrom:f1_b \[^\\)\]+\\) map\\(tofrom:f1_a" 1 "gimple" } }
      F1_B(1) = F1_A;
!$ACC END PARALLEL
!$ACC END DATA
      END SUBROUTINE F1

      SUBROUTINE F2
      IMPLICIT NONE
      INTEGER :: F2_A = 2
      REAL, DIMENSION (2) :: F2_B

!$ACC DATA COPYIN (F2_A) COPYOUT (F2_B)
! { dg-final { scan-tree-dump-times "omp target oacc_data map\\(to:f2_a \[^\\)\]+\\) map\\(from:f2_b" 1 "gimple" } }
!$ACC KERNELS DEFAULT (NONE)
! { dg-final { scan-tree-dump-times "omp target oacc_kernels default\\(none\\) map\\(tofrom:f2_b \[^\\)\]+\\) map\\(tofrom:f2_a" 1 "gimple" } }
      F2_B(1) = F2_A;
!$ACC END KERNELS
!$ACC PARALLEL DEFAULT (NONE)
! { dg-final { scan-tree-dump-times "omp target oacc_parallel default\\(none\\) map\\(tofrom:f2_b \[^\\)\]+\\) map\\(tofrom:f2_a" 1 "gimple" } }
      F2_B(1) = F2_A;
!$ACC END PARALLEL
!$ACC END DATA
      END SUBROUTINE F2

      SUBROUTINE F2_
      IMPLICIT NONE
      INTEGER :: F2__A = 2
      REAL, DIMENSION (2) :: F2__B

!$ACC DATA DEFAULT (NONE) COPYIN (F2__A) COPYOUT (F2__B)
! { dg-final { scan-tree-dump-times "omp target oacc_data map\\(to:f2__a \[^\\)\]+\\) map\\(from:f2__b \[^\\)\]+\\) default\\(none\\)" 1 "gimple" } }
!$ACC KERNELS
! { dg-final { scan-tree-dump-times "omp target oacc_kernels map\\(tofrom:f2__b \[^\\)\]+\\) map\\(tofrom:f2__a" 1 "gimple" } }
      F2__B(1) = F2__A;
!$ACC END KERNELS
!$ACC PARALLEL
! { dg-final { scan-tree-dump-times "omp target oacc_parallel map\\(tofrom:f2__b \[^\\)\]+\\) map\\(tofrom:f2__a" 1 "gimple" } }
      F2__B(1) = F2__A;
!$ACC END PARALLEL
!$ACC END DATA
      END SUBROUTINE F2_

      SUBROUTINE F3
      IMPLICIT NONE
      INTEGER :: F3_A = 2
      REAL, DIMENSION (2) :: F3_B

!$ACC DATA COPYIN (F3_A) COPYOUT (F3_B)
! { dg-final { scan-tree-dump-times "omp target oacc_data map\\(to:f3_a \[^\\)\]+\\) map\\(from:f3_b" 1 "gimple" } }
!$ACC KERNELS DEFAULT (PRESENT)
! { dg-final { scan-tree-dump-times "omp target oacc_kernels default\\(present\\) map\\(tofrom:f3_b \[^\\)\]+\\) map\\(tofrom:f3_a" 1 "gimple" } }
      F3_B(1) = F3_A;
!$ACC END KERNELS
!$ACC PARALLEL DEFAULT (PRESENT)
! { dg-final { scan-tree-dump-times "omp target oacc_parallel default\\(present\\) map\\(tofrom:f3_b \[^\\)\]+\\) map\\(tofrom:f3_a" 1 "gimple" } }
      F3_B(1) = F3_A;
!$ACC END PARALLEL
!$ACC END DATA
      END SUBROUTINE F3

      SUBROUTINE F3_
      IMPLICIT NONE
      INTEGER :: F3__A = 2
      REAL, DIMENSION (2) :: F3__B

!$ACC DATA DEFAULT (PRESENT) COPYIN (F3__A) COPYOUT (F3__B)
! { dg-final { scan-tree-dump-times "omp target oacc_data map\\(to:f3__a \[^\\)\]+\\) map\\(from:f3__b \[^\\)\]+\\) default\\(present\\)" 1 "gimple" } }
!$ACC KERNELS
! { dg-final { scan-tree-dump-times "omp target oacc_kernels map\\(tofrom:f3__b \[^\\)\]+\\) map\\(tofrom:f3__a" 1 "gimple" } }
      F3__B(1) = F3__A;
!$ACC END KERNELS
!$ACC PARALLEL
! { dg-final { scan-tree-dump-times "omp target oacc_parallel map\\(tofrom:f3__b \[^\\)\]+\\) map\\(tofrom:f3__a" 1 "gimple" } }
      F3__B(1) = F3__A;
!$ACC END PARALLEL
!$ACC END DATA
      END SUBROUTINE F3_
