       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  compiled-datetime.
           03  compiled-date.
               05  millennium PIC X.
               05  FILLER    PIC X(15).
           03  timezone  PIC X(5).
       PROCEDURE        DIVISION.
           *> Check millennium.
           MOVE FUNCTION WHEN-COMPILED TO compiled-datetime.
           IF millennium NOT = "2"
              DISPLAY "Millennium NOT OK: " millennium
              END-DISPLAY
           END-IF.

           *> Check timezone.
           IF timezone NOT = FUNCTION CURRENT-DATE (17:5)
              DISPLAY "Timezone NOT OK: " timezone
              END-DISPLAY
           END-IF.

           *> Check date format.
           INSPECT compiled-date CONVERTING "0123456789"
               TO "9999999999".
           IF compiled-date NOT = ALL "9"
               DISPLAY "Date format NOT OK: " compiled-date
               END-DISPLAY
           END-IF.

           *> Check timezone format.
           IF timezone NOT = "00000"
               INSPECT timezone CONVERTING "0123456789"
                   TO "9999999999"
               IF timezone NOT = "+9999" AND "-9999"
                   DISPLAY "Timezone format NOT OK: " timezone
                   END-DISPLAY
               END-IF
           END-IF.

           STOP RUN.

