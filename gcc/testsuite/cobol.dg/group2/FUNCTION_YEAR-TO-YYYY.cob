       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Z            USAGE BINARY-LONG.
       PROCEDURE        DIVISION.
           MOVE FUNCTION YEAR-TO-YYYY ( 50 ) TO Z.
           IF Z NOT = 2050
              DISPLAY Z
              END-DISPLAY
           END-IF.
           STOP RUN.

