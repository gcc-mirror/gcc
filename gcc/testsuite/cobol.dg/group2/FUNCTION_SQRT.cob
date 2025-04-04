       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Y   PIC   S9V9(33).
       PROCEDURE        DIVISION.
           MOVE FUNCTION SQRT ( 1.5 ) TO Y.
           IF Y NOT = 1.224744871391589049098642037352945
                   DISPLAY Y
                   END-DISPLAY
           END-IF.
           STOP RUN.

