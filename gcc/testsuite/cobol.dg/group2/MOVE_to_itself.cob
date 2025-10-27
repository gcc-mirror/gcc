       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 X             PIC 99 VALUE 12.
       PROCEDURE        DIVISION.
           MOVE X TO X.
           IF X NOT = 12
              DISPLAY X NO ADVANCING
              END-DISPLAY
           END-IF.
           STOP RUN.

