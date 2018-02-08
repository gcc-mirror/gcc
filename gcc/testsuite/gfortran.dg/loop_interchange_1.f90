! { dg-do compile }
! { dg-additional-options "-O -Wfrontend-loop-interchange" }
PROGRAM TEST_DO_SPEED
  IMPLICIT NONE

  REAL, ALLOCATABLE :: A(:,:,:), B(:,:,:), C(:,:,:)
  REAL :: TIC
  INTEGER :: T0, T1, T2
  INTEGER :: I, J, K
  INTEGER, PARAMETER :: L = 512, M = 512, N = 512

  ALLOCATE( A(L,M,N), B(L,M,N), C(L,M,N) )
  CALL RANDOM_NUMBER(A)
  CALL RANDOM_NUMBER(B)

  CALL SYSTEM_CLOCK( T0, TIC)

  DO CONCURRENT( K=1:N, J=1:M, I=1:L) ! { dg-warning "Interchanging loops" }
    C(I,J,K) = A(I,J,K) +B(I,J,K)
  END DO
END

