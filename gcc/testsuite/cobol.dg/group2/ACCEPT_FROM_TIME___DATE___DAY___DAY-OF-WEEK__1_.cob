       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       *> one byte longer to make sure there is no garbage in
       01 X             PIC X(9).
       PROCEDURE        DIVISION.
           ACCEPT X FROM TIME
           END-ACCEPT
           IF X (1:2) >= "00" AND <= "23" AND
              X (3:2) >= "00" AND <= "59" AND
              X (5:2) >= "00" AND <= "60" AND
              X (7:2) >= "00" AND <= "99" AND
              X (9: )  = SPACE
              CONTINUE
           ELSE
              DISPLAY "TIME " X "!"
              END-DISPLAY
           END-IF
           ACCEPT X FROM DATE
           END-ACCEPT
           INSPECT X CONVERTING "012345678" TO "999999999"
           IF X NOT = "999999"
              DISPLAY "DATE " X "!"
              END-DISPLAY
           END-IF
           ACCEPT X FROM DATE YYYYMMDD
           END-ACCEPT
           INSPECT X CONVERTING "012345678" TO "999999999"
           IF X NOT = "99999999"
              DISPLAY "YYYYMMDD " X "!"
              END-DISPLAY
           END-IF
           ACCEPT X FROM DAY
           END-ACCEPT
           INSPECT X CONVERTING "012345678" TO "999999999"
           IF X NOT = "99999"
              DISPLAY "DAY " X "!"
              END-DISPLAY
           END-IF
           ACCEPT X FROM DAY YYYYDDD
           END-ACCEPT
           INSPECT X CONVERTING "012345678" TO "999999999"
           IF X NOT = "9999999"
              DISPLAY "YYYYDDD " X "!"
              END-DISPLAY
           END-IF
           ACCEPT X FROM DAY-OF-WEEK
           END-ACCEPT
           INSPECT X CONVERTING "1234567" TO "9999999"
           IF X NOT = "9"
              DISPLAY "DAY-OF-WEEK " X "!"
              END-DISPLAY
           END-IF
           STOP RUN.

