! demonstrates basic direct access using variables for REC
! pr14872
       OPEN(UNIT=10,ACCESS='DIRECT',RECL=128)
       DO I = 1,10
          WRITE(10,REC=I,ERR=10)I
       ENDDO
       CLOSE(10)
       OPEN(UNIT=10,ACCESS='DIRECT',RECL=128)
       DO I = 1,10
          READ(10,REC=I,ERR=10)J
          IF (J.NE.I) THEN
!           PRINT*,' READ ',J,' EXPECTED ',I
            CALL ABORT
          ENDIF
       ENDDO
       STOP
 10    CONTINUE
!      PRINT*,' ERR= RETURN FROM READ OR WRITE'
       CALL ABORT
       END
