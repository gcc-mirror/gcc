       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Y   PIC      9(8)   COMP-5.
       PROCEDURE        DIVISION.
           MOVE FUNCTION SECONDS-PAST-MIDNIGHT TO Y.
           IF Y NOT < 86402
                   DISPLAY Y
                   END-DISPLAY
           END-IF.
           STOP RUN.

