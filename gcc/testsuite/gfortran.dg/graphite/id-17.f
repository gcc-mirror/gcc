      SUBROUTINE SPECTOP(Dr,N)
      DIMENSION d1(0:32,0:32) , Dr(0:32,0:32) , x(0:32)
      DO k = 0 , N
         fctr2 = o
         DO j = 0 , N
            fctr = fctr1*fctr2
            IF ( j.NE.k ) THEN
               d1(k,j) = ck*fctr/(cj*(x(k)-x(j)))
            ENDIF
            fctr2 = -o*fctr2
         ENDDO
         DO j = 0 , N
            Dr(k,j) = d1(N-k,N-j)
         ENDDO
      ENDDO
      END
