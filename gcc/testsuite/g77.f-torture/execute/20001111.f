      DOUBLE PRECISION VALUE(2), TOLD, BK
      DATA VALUE /0D0, 1D0/
      DATA TOLD /0D0/
      DO I=1, 2
         BK = VALUE(I)
         IF(BK .GT. TOLD) GOTO 10
      ENDDO
      WRITE(*,*)'Error: BK = ', BK
      CALL ABORT
 10   CONTINUE
      WRITE(*,*)'No Error: BK = ', BK
      END
