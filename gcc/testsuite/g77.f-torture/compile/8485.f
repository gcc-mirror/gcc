C      Extracted from PR fortran/8485
       PARAMETER (PPMULT = 1.0E5)
       INTEGER*8 NWRONG
       PARAMETER (NWRONG = 8)
       PARAMETER (DDMULT = PPMULT * NWRONG)
       PRINT 10, DDMULT
10     FORMAT (F10.3)
       END
