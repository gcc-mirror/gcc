       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Y   PIC   9V9(32).
       PROCEDURE        DIVISION.
           MOVE    FUNCTION PI TO Y.
           IF Y NOT = 3.14159265358979323846264338327950
              DISPLAY Y
              END-DISPLAY
           END-IF.
           STOP RUN.

