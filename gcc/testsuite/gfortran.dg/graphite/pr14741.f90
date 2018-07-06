! { dg-options "-O3 -ffast-math -floop-nest-optimize -floop-block -fdump-tree-graphite-all" }

  INTEGER, PARAMETER :: N=1024
  REAL*8 :: A(N,N), B(N,N), C(N,N)
  REAL*8 :: t1,t2
  A=0.1D0
  B=0.1D0
  C=0.0D0
  CALL cpu_time(t1)
  CALL mult(A,B,C,N)
  CALL cpu_time(t2)
  write(6,*) t2-t1,C(1,1)
END program

SUBROUTINE mult(A,B,C,N)
  REAL*8 :: A(N,N), B(N,N), C(N,N)
  INTEGER :: I,J,K,N
  DO J=1,N
     DO I=1,N
        DO K=1,N
           C(I,J)=C(I,J)+A(I,K)*B(K,J)
        ENDDO
     ENDDO
  ENDDO
END SUBROUTINE mult

! { dg-final { scan-tree-dump "tiled by" "graphite" } }
