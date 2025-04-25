       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  FILLER       PIC X VALUE SPACE.
           88 X         VALUE "X".
       PROCEDURE        DIVISION.
           IF X
               DISPLAY "NOT OK"
               END-DISPLAY
           END-IF
           SET X TO TRUE.
           IF NOT X
               DISPLAY "NOT OK"
               END-DISPLAY
           END-IF
           STOP RUN.

