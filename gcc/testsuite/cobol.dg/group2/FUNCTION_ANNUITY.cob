       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Z   PIC   S9V9(33).
       PROCEDURE        DIVISION.
           MOVE FUNCTION ANNUITY ( 3, 5 ) TO Z.
           IF Z NOT = 3.002932551319648093841642228739003
              DISPLAY Z
              END-DISPLAY
           END-IF.
           STOP RUN.

