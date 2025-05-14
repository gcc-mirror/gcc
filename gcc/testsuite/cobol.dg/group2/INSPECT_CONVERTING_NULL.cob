       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 X             PIC X(3) VALUE LOW-VALUES.
       PROCEDURE        DIVISION.
           INSPECT X CONVERTING NULL TO "A".
           IF X NOT = "AAA"
              DISPLAY X NO ADVANCING
              END-DISPLAY
           END-IF.
           STOP RUN.

