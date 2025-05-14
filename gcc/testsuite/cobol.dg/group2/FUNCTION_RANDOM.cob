       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Y   PIC   S99V99   COMP VALUE -1.0.
       PROCEDURE        DIVISION.
           MOVE FUNCTION RANDOM ( ) TO Y.
           IF Y < 0
                   DISPLAY Y
                   END-DISPLAY
           END-IF.
           STOP RUN.

