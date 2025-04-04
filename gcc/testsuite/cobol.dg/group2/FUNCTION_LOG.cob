       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Y   PIC   S9V9(33).
       PROCEDURE        DIVISION.
           MOVE FUNCTION LOG ( 1.5 ) TO Y.
           IF Y NOT = 0.405465108108164381978013115464349
              DISPLAY Y
              END-DISPLAY
           END-IF.
           STOP RUN.

