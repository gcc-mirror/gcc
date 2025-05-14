       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Y   PIC   S9V9(32).
       PROCEDURE        DIVISION.
           MOVE FUNCTION STANDARD-DEVIATION ( 3 -14 0 8 -3 ) TO Y.
           IF Y NOT = 7.35934779718963954877237043574538
                   DISPLAY Y
                   END-DISPLAY
           END-IF.
           STOP RUN.

