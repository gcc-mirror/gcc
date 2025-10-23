       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 G             VALUE "1234".
         02 X           PIC X OCCURS 4.
       01 Z             PIC X.
       PROCEDURE        DIVISION.
           MOVE X((3 + 1) / 2) TO Z.
           IF Z NOT = "2"
              DISPLAY Z
              END-DISPLAY
           END-IF.
           MOVE X(2 ** 2) TO Z.
           IF Z NOT = "4"
              DISPLAY Z
              END-DISPLAY
           END-IF.
           STOP RUN.

