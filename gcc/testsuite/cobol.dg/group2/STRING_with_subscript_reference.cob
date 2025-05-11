       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  G.
           02 X         PIC X(3) OCCURS 3.
       PROCEDURE        DIVISION.
           MOVE   SPACES TO G.
           STRING "abc" INTO X(2)
           END-STRING.
           IF G NOT = "   abc   "
              DISPLAY X(1) NO ADVANCING
              END-DISPLAY
           END-IF.
           STOP RUN.

