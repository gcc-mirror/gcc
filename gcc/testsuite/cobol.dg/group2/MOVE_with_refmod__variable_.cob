       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 X             PIC X(4) VALUE "1234".
       01 Y             PIC X(4) VALUE "abcd".
       01 I             PIC 9 VALUE 1.
       PROCEDURE        DIVISION.
           MOVE X(1:I) TO Y.
           IF Y NOT = "1   "
              DISPLAY Y NO ADVANCING
              END-DISPLAY
           END-IF.
           STOP RUN.

