      SUBROUTINE STONUM(STRVAR,LENGTH)
      CHARACTER STRVAR*(*) , CHK
      LOGICAL MEND , NMARK , MMARK , EMARK
      NMARK = .FALSE.
      MMARK = .FALSE.
      DO WHILE ( .NOT.MEND )
            IF ( CHK.GE.'0' .AND. CHK.LE.'9' ) THEN
               IF ( CHK.EQ.'E' ) THEN
                  NMARK = .TRUE.
               ELSEIF ( .NOT.MMARK .AND. CHK.EQ.'*' .AND. .NOT.NMARK )  &
     &                  THEN
                  MMARK = .TRUE.
               ENDIF
            ENDIF
      ENDDO
      END

