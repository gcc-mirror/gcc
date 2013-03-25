! { dg-do compile }
!
! PR fortran/39288
!
! From IR F03/0129, cf.
! Fortran 2003, Technical Corrigendum 5
!
! Was invalid before.

  SUBROUTINE S(A,I,K)
    USE ISO_C_BINDING
    CHARACTER(*),TARGET :: A
    CHARACTER(:),ALLOCATABLE,TARGET :: B
    TYPE(C_PTR) P1,P2,P3,P4,P5
    P1 = C_LOC(A(1:1))    ! *1
    P2 = C_LOC(A(I:I))    ! *2
    P3 = C_LOC(A(1:))     ! *3
    P4 = C_LOC(A(I:K))    ! *4
    ALLOCATE(CHARACTER(1)::B)
    P5 = C_LOC(B)         ! *5
  END SUBROUTINE
