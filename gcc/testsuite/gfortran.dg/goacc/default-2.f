! OpenACC default clause: invalid syntax.

      SUBROUTINE F1
      IMPLICIT NONE

!$ACC KERNELS DEFAULT ! { dg-error "Unclassifiable OpenACC directive" }
!$ACC END KERNELS ! { dg-error "Unexpected" }
!$ACC PARALLEL DEFAULT ! { dg-error "Unclassifiable OpenACC directive" }
!$ACC END PARALLEL ! { dg-error "Unexpected" }

!$ACC KERNELS DEFAULT ( ! { dg-error "Unclassifiable OpenACC directive" }
!$ACC END KERNELS ! { dg-error "Unexpected" }
!$ACC PARALLEL DEFAULT ( ! { dg-error "Unclassifiable OpenACC directive" }
!$ACC END PARALLEL ! { dg-error "Unexpected" }

!$ACC KERNELS DEFAULT (, ! { dg-error "Unclassifiable OpenACC directive" }
!$ACC END KERNELS ! { dg-error "Unexpected" }
!$ACC PARALLEL DEFAULT (, ! { dg-error "Unclassifiable OpenACC directive" }
!$ACC END PARALLEL ! { dg-error "Unexpected" }

!$ACC KERNELS DEFAULT () ! { dg-error "Unclassifiable OpenACC directive" }
!$ACC END KERNELS ! { dg-error "Unexpected" }
!$ACC PARALLEL DEFAULT () ! { dg-error "Unclassifiable OpenACC directive" }
!$ACC END PARALLEL ! { dg-error "Unexpected" }

!$ACC KERNELS DEFAULT (,) ! { dg-error "Unclassifiable OpenACC directive" }
!$ACC END KERNELS ! { dg-error "Unexpected" }
!$ACC PARALLEL DEFAULT (,) ! { dg-error "Unclassifiable OpenACC directive" }
!$ACC END PARALLEL ! { dg-error "Unexpected" }

!$ACC KERNELS DEFAULT (FIRSTPRIVATE) ! { dg-error "Unclassifiable OpenACC directive" }
!$ACC END KERNELS ! { dg-error "Unexpected" }
!$ACC PARALLEL DEFAULT (FIRSTPRIVATE) ! { dg-error "Unclassifiable OpenACC directive" }
!$ACC END PARALLEL ! { dg-error "Unexpected" }

!$ACC KERNELS DEFAULT (PRIVATE) ! { dg-error "Unclassifiable OpenACC directive" }
!$ACC END KERNELS ! { dg-error "Unexpected" }
!$ACC PARALLEL DEFAULT (PRIVATE) ! { dg-error "Unclassifiable OpenACC directive" }
!$ACC END PARALLEL ! { dg-error "Unexpected" }

!$ACC KERNELS DEFAULT (SHARED) ! { dg-error "Unclassifiable OpenACC directive" }
!$ACC END KERNELS ! { dg-error "Unexpected" }
!$ACC PARALLEL DEFAULT (SHARED) ! { dg-error "Unclassifiable OpenACC directive" }
!$ACC END PARALLEL ! { dg-error "Unexpected" }

!$ACC KERNELS DEFAULT (NONE ! { dg-error "Unclassifiable OpenACC directive" }
!$ACC END KERNELS ! { dg-error "Unexpected" }
!$ACC PARALLEL DEFAULT (NONE ! { dg-error "Unclassifiable OpenACC directive" }
!$ACC END PARALLEL ! { dg-error "Unexpected" }

!$ACC KERNELS DEFAULT (NONE NONE) ! { dg-error "Unclassifiable OpenACC directive" }
!$ACC END KERNELS ! { dg-error "Unexpected" }
!$ACC PARALLEL DEFAULT (NONE NONE) ! { dg-error "Unclassifiable OpenACC directive" }
!$ACC END PARALLEL ! { dg-error "Unexpected" }

!$ACC KERNELS DEFAULT (NONE, NONE) ! { dg-error "Unclassifiable OpenACC directive" }
!$ACC END KERNELS ! { dg-error "Unexpected" }
!$ACC PARALLEL DEFAULT (NONE, NONE) ! { dg-error "Unclassifiable OpenACC directive" }
!$ACC END PARALLEL ! { dg-error "Unexpected" }
      END SUBROUTINE F1
