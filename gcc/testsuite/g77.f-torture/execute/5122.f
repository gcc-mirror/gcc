      CHARACTER*20 PARTD(6)
      INTEGER*2 L
      DATA (PARTD(L),L=1,6)/'A','B','C','D','E','F'/
      IF (    PARTD(1) .NE. 'A' .OR. PARTD(2) .NE. 'B'
     ,   .OR. PARTD(3) .NE. 'C' .OR. PARTD(4) .NE. 'D'
     ,   .OR. PARTD(5) .NE. 'E' .OR. PARTD(6) .NE. 'F')
     ,      CALL ABORT
      END
