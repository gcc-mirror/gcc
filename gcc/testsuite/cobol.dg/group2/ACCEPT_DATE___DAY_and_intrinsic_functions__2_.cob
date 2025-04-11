       *> { dg-do run }
       *> { dg-set-target-env-var GCOBOL_CURRENT_DATE "2020/06/12 18:45:22" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       *> one byte longer to make sure there is no garbage in
       01  WS-YYYYMMDD   PIC 9(9).
       01  WS-YYYYDDD    PIC 9(8).
       PROCEDURE        DIVISION.
           ACCEPT WS-YYYYMMDD FROM DATE YYYYMMDD
           END-ACCEPT
           ACCEPT WS-YYYYDDD  FROM DAY  YYYYDDD
           END-ACCEPT
           IF    FUNCTION INTEGER-OF-DATE (WS-YYYYMMDD)
           NOT = FUNCTION INTEGER-OF-DAY  (WS-YYYYDDD)
              DISPLAY "DIFFERENCES FOUND!"
              END-DISPLAY
              DISPLAY "YYYYMMDD = " WS-YYYYMMDD ", "
                      "YYYYDDD = " WS-YYYYDDD
              END-DISPLAY
              DISPLAY "INTEGER-OF-DATE = "
                      FUNCTION INTEGER-OF-DATE (WS-YYYYMMDD) ", "
                      "INTEGER-OF-DAY = "
                      FUNCTION INTEGER-OF-DAY  (WS-YYYYDDD)
              END-DISPLAY
              MOVE 1 TO RETURN-CODE
           END-IF
           STOP RUN.

