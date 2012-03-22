! { dg-do compile }
!
! PR fortran/52452
!
! Contributed by Roger Ferrer Ibanez
!
PROGRAM test_etime
    IMPLICIT NONE
    INTRINSIC :: etime
    REAL(4) :: tarray(1:2)
    REAL(4) :: result

    CALL etime(tarray, result)
END PROGRAM test_etime

subroutine test_etime2
    IMPLICIT NONE
    INTRINSIC :: etime
    REAL(4) :: tarray(1:2)
    REAL(4) :: result

    result = etime(tarray)
END subroutine test_etime2
