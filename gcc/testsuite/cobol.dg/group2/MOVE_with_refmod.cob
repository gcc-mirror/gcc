       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 X             PIC 9(4) VALUE 0.
       PROCEDURE        DIVISION.
           MOVE "1" TO X(1:1).
           IF X NOT = 1000
              DISPLAY X NO ADVANCING
              END-DISPLAY
           END-IF.
           STOP RUN.

