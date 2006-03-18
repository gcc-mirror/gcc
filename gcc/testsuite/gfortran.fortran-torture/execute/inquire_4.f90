! pr 14904
! inquire lastrec not correct when two records written 
! with one write statement
       OPEN(UNIT=10,ACCESS='DIRECT',FORM='FORMATTED',RECL=120)
 100   FORMAT(I4)
       WRITE(UNIT=10,REC=1,FMT=100)1
       INQUIRE(UNIT=10,NEXTREC=J)
       IF (J.NE.2) THEN
!          PRINT*,'NEXTREC RETURNED ',J,' EXPECTED 2'
           CALL ABORT
       ENDIF
 200   FORMAT(I4,/,I4)
       WRITE(UNIT=10,REC=2,FMT=200)2,3
       INQUIRE(UNIT=10,NEXTREC=J)
       IF (J.NE.4) THEN
!          PRINT*,'NEXTREC RETURNED ',J,' EXPECTED 4'
           CALL ABORT
       ENDIF
       CLOSE(UNIT=10,STATUS='DELETE')
       END

