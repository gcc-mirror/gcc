       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X   PIC   X(10) VALUE "a#b.c%d+e$".
       01  Z   PIC   X(4).
       PROCEDURE        DIVISION.
           MOVE FUNCTION UPPER-CASE ( X ) (1 : 3) TO Z.
           IF Z NOT = "A#B "
              DISPLAY Z
              END-DISPLAY
           END-IF.
           STOP RUN.

