       IMPLICIT NONE
C properly handle a "/" in a $<NAME> $END namelist
C pr12884 --
C error in reading a namelist when it is preceded by a line with a SLASH
C
       CHARACTER*80 DL(7)
       DATA DL /'$file',
     1          'oms omsmc.i2',
     2          'pseu pseudo/PSN',
     3          '$end',
     4          '$CNTRL',
     5          'ispher=1,NOSYM=2,RUNFLG=3,noprop=4,',
     6          '$END'/
C $file is not a valid namelist, but it still
C is parsed by the runtime
       INTEGER*4 ISPHER,NOSYM,RUNFLG,NOPROP /-1 /
       INTEGER I
       NAMELIST /CNTRL/ ISPHER,NOSYM,RUNFLG,NOPROP
C make a unique datafile
       OPEN(UNIT=9,STATUS='SCRATCH')
       WRITE(9,*,ERR=100)(DL(I),I=1,7)
       REWIND(9)
       READ(9,NML=CNTRL,ERR=100)
       CLOSE(9)
       IF (ISPHER.NE.1.OR.NOSYM.NE.2.OR.RUNFLG.NE.3.OR.NOPROP.NE.4)THEN
           CALL ABORT
       ENDIF
C all is well at this point !!
       STOP
 100   PRINT*,'FILE ERROR(S)'
       CALL ABORT 
       END
