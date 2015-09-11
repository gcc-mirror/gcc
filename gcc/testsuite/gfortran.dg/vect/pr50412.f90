! { dg-do compile }

      DOUBLE PRECISION AK,AI,AAE
      COMMON/com/AK(36),AI(4,4),AAE(8,4),ii,jj
      DO 20 II=1,4
        DO 21 JJ=1,4
          AK(n)=AK(n)-AAE(I,II)*AI(II,JJ)
   21   CONTINUE
   20 CONTINUE
      END

