       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Y            PIC 9(12)      VALUE 600851475143.
       01  R            PIC S9(4)V9(4) VALUE 0.
       PROCEDURE        DIVISION.
           MOVE FUNCTION MOD ( -11 5 ) TO R
           IF R NOT = 4
              DISPLAY 'first one wrong: ' R
              END-DISPLAY
           END-IF
           MOVE FUNCTION MOD ( Y, 71 ) TO R
           IF R NOT = 0
              DISPLAY 'second one wrong: ' R
              END-DISPLAY
           END-IF
           STOP RUN.

