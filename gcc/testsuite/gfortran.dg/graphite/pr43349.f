! { dg-options "-O2 -floop-nest-optimize" }

      SUBROUTINE BUG(A,B,X,Y,Z,N)
      IMPLICIT NONE
      DOUBLE PRECISION A(*),B(*),X(*),Y(*),Z(*)
      INTEGER N,J,K
      K = 0
      DO J = 1,N
         K = K+1
         X(K) = B(J+N*7)
         Y(K) = B(J+N*8)
         Z(K) = B(J+N*2)  + A(J+N*2)
         K = K+1
         X(K) = B(J+N*3)  + A(J+N*3)
         Y(K) = B(J+N*9)  + A(J)
         Z(K) = B(J+N*15)
         K = K+1
         X(K) = B(J+N*4)  + A(J+N*4)
         Y(K) = B(J+N*15)
         Z(K) = B(J+N*10) + A(J)
         K = K+1
         X(K) = B(J+N*11) + A(J+N)
         Y(K) = B(J+N*5)  + A(J+N*5)
         Z(K) = B(J+N*16)
         K = K+1
         X(K) = B(J+N*16)
         Y(K) = B(J+N*6)  + A(J+N*6)
         Z(K) = B(J+N*12) + A(J+N)
         K = K+1
         X(K) = B(J+N*13) + A(J+N*2)
         Y(K) = B(J+N*17)
         Z(K) = B(J+N*7)  + A(J+N*7)
      ENDDO
      RETURN
      END
