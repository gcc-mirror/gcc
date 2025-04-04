       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Y   PIC   9V9(33).
       PROCEDURE        DIVISION.
           MOVE    FUNCTION E TO Y.
           IF Y NOT = 2.718281828459045235360287471352662
              DISPLAY Y
              END-DISPLAY
           END-IF.
           STOP RUN.

