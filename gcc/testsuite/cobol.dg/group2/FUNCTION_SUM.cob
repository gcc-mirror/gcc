       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Z            USAGE BINARY-LONG.
       PROCEDURE        DIVISION.
           MOVE FUNCTION SUM ( 3 -14 0 8 -3 ) TO Z.
           IF Z NOT = -6
              DISPLAY Z
              END-DISPLAY
           END-IF.
           STOP RUN.

