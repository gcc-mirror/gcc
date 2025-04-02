       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 X             PIC X(3) VALUE "BCA".
       PROCEDURE        DIVISION.
           INSPECT X REPLACING ALL "BC" BY SPACE.
           IF X NOT = "  A"
              DISPLAY X NO ADVANCING
              END-DISPLAY
           END-IF.
           STOP RUN.

