      SUBROUTINE CHOUT(CHR,ICNT)
C ICE: failed assertion `expr != NULL'
C Reduced version of GNATS PR fortran/329 from trond.bo@dnmi.no
      INTEGER CHR(ICNT)
      CHARACTER*255 BUF
      BUF(1:1)=CHAR(CHR(1))
      CALL FPUTC(1,BUF(1:1))
      RETURN
      END
