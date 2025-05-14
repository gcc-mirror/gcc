       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  R            PIC S9(4)V9(4) COMP-5 VALUE 0.
       PROCEDURE        DIVISION.
           MOVE FUNCTION REM ( -11 5 ) TO R
           IF R NOT = -1
              DISPLAY R END-DISPLAY
           END-IF
           STOP RUN.

