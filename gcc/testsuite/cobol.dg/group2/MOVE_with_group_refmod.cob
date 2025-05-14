       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 G.
         02 X           PIC 9999 VALUE 1234.
       PROCEDURE        DIVISION.
           MOVE "99" TO G(3:2).
           IF G NOT = "1299"
              DISPLAY G NO ADVANCING
              END-DISPLAY
           END-IF.
           STOP RUN.

