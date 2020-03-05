! { dg-do compile }
!
! PR fortran/63744
! duplicate use rename used to be rejected when the target name
! was that of the current program unit 
!
! Original testcase from Roger Ferrer Ibanez <roger.ferrer@bsc.es>

MODULE MOO
    INTEGER :: A, B, C, D, E, F, G, H, I
END MODULE MOO

SUBROUTINE S
    USE MOO, ONLY: X => A, X => A
END SUBROUTINE S

SUBROUTINE T
    USE MOO, ONLY: X => B
    USE MOO, ONLY: X => B
END SUBROUTINE T

SUBROUTINE C          ! { dg-error "\(1\)" }
    USE MOO, ONLY: C  ! { dg-error "conflicts with the" }
END SUBROUTINE C

SUBROUTINE D
    USE MOO, ONLY: X => D
END SUBROUTINE D

SUBROUTINE E
    USE MOO, ONLY: X => E, X => E
END SUBROUTINE E

SUBROUTINE F
    USE MOO, ONLY: X => F
    USE MOO, ONLY: X => F
END SUBROUTINE F

SUBROUTINE X              ! { dg-error "\(1\)" }
    USE MOO, ONLY: X => G ! { dg-error "conflicts with the rename" }
END SUBROUTINE X

SUBROUTINE Y              ! { dg-error "\(1\)" }
    USE MOO, ONLY: Y => H ! { dg-error "conflicts with the rename" }
END SUBROUTINE Y

SUBROUTINE Z                        ! { dg-error "\(1\)" }
    USE MOO, ONLY: Z => I, Z => I   ! { dg-error "conflicts with the rename" }
END SUBROUTINE Z

