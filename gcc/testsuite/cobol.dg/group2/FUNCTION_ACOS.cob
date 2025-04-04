       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Z   PIC   S9V9(33).
       PROCEDURE        DIVISION.
           MOVE FUNCTION ACOS ( -0.2345 ) TO Z.
           IF Z NOT = 1.807500521108243435101500438523210
              DISPLAY Z
              END-DISPLAY
           END-IF.
           STOP RUN.

