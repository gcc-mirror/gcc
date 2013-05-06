! { dg-do compile }
!
! PR fortran/57141
!
! Contributed by Roger Ferrer Ibanez 
!
MODULE M
    INTRINSIC :: NULL
END MODULE M

MODULE M_INTERN
    USE M
    IMPLICIT NONE
    REAL, POINTER :: ARR(:) => NULL()
END MODULE M_INTERN
