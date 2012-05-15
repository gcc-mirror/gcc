! PR middle-end/43866
! { dg-do run }
! { dg-options "-funswitch-loops -fbounds-check" }

MODULE PR43866
  IMPLICIT NONE
  TYPE TT
    REAL(KIND=4), DIMENSION(:,:), POINTER :: A
    REAL(KIND=8), DIMENSION(:,:), POINTER :: B
  END TYPE
CONTAINS
  SUBROUTINE FOO(M,X,Y,T)
    TYPE(TT), POINTER :: M
    INTEGER, INTENT(IN) :: Y, X
    INTEGER :: C, D
    LOGICAL :: T
    REAL(KIND = 4), DIMENSION(:,:), POINTER :: P
    REAL(KIND = 8), DIMENSION(:,:), POINTER :: Q

    Q => M%B
    P => M%A
    DO C=1,X
      DO D=C+1,Y
        IF (T) THEN
          P(D,C)=P(C,D)
        ELSE
          Q(D,C)=Q(C,D)
        ENDIF
      ENDDO
    ENDDO
  END SUBROUTINE FOO
END MODULE PR43866

  USE PR43866
  TYPE(TT), POINTER :: Q
  INTEGER, PARAMETER :: N=17
  ALLOCATE (Q)
  NULLIFY (Q%A)
  ALLOCATE (Q%B(N,N))
  Q%B=0
  CALL FOO (Q,N,N,.FALSE.)
END
