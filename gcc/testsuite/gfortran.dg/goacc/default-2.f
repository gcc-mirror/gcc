! OpenACC default clause: invalid syntax.

      SUBROUTINE F1
      IMPLICIT NONE

!$ACC KERNELS DEFAULT ! { dg-error "Expected '\\(' after 'default" }
!$ACC END KERNELS ! { dg-error "Unexpected" }
!$ACC PARALLEL DEFAULT ! { dg-error "Expected '\\(' after 'default" }
!$ACC END PARALLEL ! { dg-error "Unexpected" }

!$ACC KERNELS DEFAULT ( ! { dg-error "Expected NONE or PRESENT in DEFAULT clause" }
!$ACC END KERNELS ! { dg-error "Unexpected" }
!$ACC PARALLEL DEFAULT ( ! { dg-error "Expected NONE or PRESENT in DEFAULT clause" }
!$ACC END PARALLEL ! { dg-error "Unexpected" }

!$ACC KERNELS DEFAULT (, ! { dg-error "Expected NONE or PRESENT in DEFAULT clause" }
!$ACC END KERNELS ! { dg-error "Unexpected" }
!$ACC PARALLEL DEFAULT (, ! { dg-error "Expected NONE or PRESENT in DEFAULT clause" }
!$ACC END PARALLEL ! { dg-error "Unexpected" }

!$ACC KERNELS DEFAULT () ! { dg-error "Expected NONE or PRESENT in DEFAULT clause" }
!$ACC END KERNELS ! { dg-error "Unexpected" }
!$ACC PARALLEL DEFAULT () ! { dg-error "Expected NONE or PRESENT in DEFAULT clause" }
!$ACC END PARALLEL ! { dg-error "Unexpected" }

!$ACC KERNELS DEFAULT (,) ! { dg-error "Expected NONE or PRESENT in DEFAULT clause" }
!$ACC END KERNELS ! { dg-error "Unexpected" }
!$ACC PARALLEL DEFAULT (,) ! { dg-error "Expected NONE or PRESENT in DEFAULT clause" }
!$ACC END PARALLEL ! { dg-error "Unexpected" }

!$ACC KERNELS DEFAULT (FIRSTPRIVATE) ! { dg-error "Expected NONE or PRESENT in DEFAULT clause" }
!$ACC END KERNELS ! { dg-error "Unexpected" }
!$ACC PARALLEL DEFAULT (FIRSTPRIVATE) ! { dg-error "Expected NONE or PRESENT in DEFAULT clause" }
!$ACC END PARALLEL ! { dg-error "Unexpected" }

!$ACC KERNELS DEFAULT (PRIVATE) ! { dg-error "Expected NONE or PRESENT in DEFAULT clause" }
!$ACC END KERNELS ! { dg-error "Unexpected" }
!$ACC PARALLEL DEFAULT (PRIVATE) ! { dg-error "Expected NONE or PRESENT in DEFAULT clause" }
!$ACC END PARALLEL ! { dg-error "Unexpected" }

!$ACC KERNELS DEFAULT (SHARED) ! { dg-error "Expected NONE or PRESENT in DEFAULT clause" }
!$ACC END KERNELS ! { dg-error "Unexpected" }
!$ACC PARALLEL DEFAULT (SHARED) ! { dg-error "Expected NONE or PRESENT in DEFAULT clause" }
!$ACC END PARALLEL ! { dg-error "Unexpected" }

!$ACC KERNELS DEFAULT (NONE ! { dg-error "Failed to match clause" }
!$ACC END KERNELS ! { dg-error "Unexpected" }
!$ACC PARALLEL DEFAULT (NONE ! { dg-error "Failed to match clause" }
!$ACC END PARALLEL ! { dg-error "Unexpected" }

!$ACC KERNELS DEFAULT (NONE NONE) ! { dg-error "Failed to match clause" }
!$ACC END KERNELS ! { dg-error "Unexpected" }
!$ACC PARALLEL DEFAULT (NONE NONE) ! { dg-error "Failed to match clause" }
!$ACC END PARALLEL ! { dg-error "Unexpected" }

!$ACC KERNELS DEFAULT (NONE, NONE) ! { dg-error "Failed to match clause" }
!$ACC END KERNELS ! { dg-error "Unexpected" }
!$ACC PARALLEL DEFAULT (NONE, NONE) ! { dg-error "Failed to match clause" }
!$ACC END PARALLEL ! { dg-error "Unexpected" }
      END SUBROUTINE F1
