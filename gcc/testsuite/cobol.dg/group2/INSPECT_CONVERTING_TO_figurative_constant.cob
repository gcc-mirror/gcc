       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 X             PIC X(3) VALUE "BCA".
       PROCEDURE        DIVISION.
           INSPECT X CONVERTING "ABC" TO SPACES.
           IF X NOT = SPACES
              DISPLAY X NO ADVANCING
              END-DISPLAY
           END-IF.
           STOP RUN.

