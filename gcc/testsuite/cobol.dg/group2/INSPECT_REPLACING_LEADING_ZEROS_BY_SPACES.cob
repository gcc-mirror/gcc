       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 X             PIC X(4) VALUE "0001".
       PROCEDURE        DIVISION.
           INSPECT X REPLACING LEADING ZEROS BY SPACES.
           IF X NOT = "   1"
              DISPLAY "Should be '   1' but is '" X "'".
           STOP RUN.

