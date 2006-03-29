! pr 14762 - '/' not working in format
       INTEGER N(5)
       DATA N/1,2,3,4,5/
       OPEN(UNIT=7)
 100   FORMAT(I4) 
       WRITE(7,100)N
       CLOSE(7) 
       OPEN(7)
 200   FORMAT(I4,///I4)
       READ(7,200)I,J
       CLOSE(7, STATUS='DELETE') 
       IF (I.NE.1) CALL ABORT
       IF (J.NE.4) CALL ABORT
       END
