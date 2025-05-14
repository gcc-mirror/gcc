       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Z            PIC S9(4)V9(4) COMP-5.
       PROCEDURE        DIVISION.
           MOVE FUNCTION VARIANCE ( 3 -14 0 8 -3 ) TO Z.
           IF Z NOT = 54.16
              DISPLAY Z
              END-DISPLAY
           END-IF.
           STOP RUN.

