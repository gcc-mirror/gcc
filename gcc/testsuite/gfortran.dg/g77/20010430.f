c { dg-do run }
      REAL DAT(2,5)
      DO I = 1, 5
         DAT(1,I) = I*1.6356-NINT(I*1.6356)
         DAT(2,I) = I
      ENDDO
      DO I = 1, 4
         DO J = I+1, 5
            IF (DAT(1,J) - DAT(1,I) .LT. 0.0) THEN
               DO K = 1, 2
                  TMP = DAT(K,I)
                  DAT(K,I) = DAT(K,J)
                  DAT(K,J) = TMP
               ENDDO
            ENDIF
         ENDDO
      ENDDO
      DO I = 1, 4
         IF (DAT(1,I) .GT. DAT(1,I+1)) CALL ABORT
      ENDDO
      END
