       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Y   PIC   S9V9(33).
       PROCEDURE        DIVISION.
           MOVE FUNCTION ATAN ( 1 ) TO Y.
           IF Y NOT = 0.785398163397448309615660845819875
              DISPLAY Y
              END-DISPLAY
           END-IF.
           STOP RUN.

