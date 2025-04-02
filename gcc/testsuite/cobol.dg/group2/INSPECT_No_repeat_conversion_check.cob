       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 X             PIC X(3) VALUE "BCA".
       01 Y             PIC X(6) VALUE "   BCA".
       PROCEDURE        DIVISION.
           INSPECT X CONVERTING "ABC" TO "BCD".
           IF X NOT = "CDB"
              DISPLAY "X: " X.
           INSPECT Y CONVERTING "ABC" TO "BCD".
           IF Y NOT = "   CDB"
              DISPLAY "Y: " Y.
           STOP RUN.

