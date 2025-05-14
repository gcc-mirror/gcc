       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 X             PIC X(4) VALUE "ABC ".
       01 TAL           PIC 999 VALUE 0.
       PROCEDURE        DIVISION.
           MOVE 0 TO TAL.
           INSPECT X TALLYING TAL FOR CHARACTERS
                     AFTER INITIAL " ".
           IF TAL NOT = 0
              DISPLAY TAL NO ADVANCING
              END-DISPLAY
           END-IF.
           MOVE 0 TO TAL.
           MOVE " ABC" TO X.
           INSPECT X TALLYING TAL FOR CHARACTERS
                     AFTER INITIAL " ".
           IF TAL NOT = 3
              DISPLAY TAL NO ADVANCING
              END-DISPLAY
           END-IF.
           STOP RUN.

