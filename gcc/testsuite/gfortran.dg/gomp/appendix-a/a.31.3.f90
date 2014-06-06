! { dg-do compile }
        PROGRAM A31_3_WRONG
        MAX = HUGE(0)
        M=0
        !$OMP PARALLEL DO REDUCTION(MAX: M) ! MAX is no longer the
                                            ! intrinsic so this
                                            ! is non-conforming
! { dg-error "OMP DECLARE REDUCTION max not found" "" { target *-*-* } 5 } */
        DO I = 1, 100
        CALL SUB(M,I)
        END DO
        END PROGRAM A31_3_WRONG
        SUBROUTINE SUB(M,I)
        M = MAX(M,I)
        END SUBROUTINE SUB
