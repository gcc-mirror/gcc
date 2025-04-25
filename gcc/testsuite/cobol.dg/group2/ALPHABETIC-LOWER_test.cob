       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X            PIC X(04) VALUE "aaaa".
       01  FILLER REDEFINES X.
           03  XBYTE    PIC X.
           03  FILLER   PIC XXX.
       PROCEDURE        DIVISION.
           MOVE X"0D"   TO XBYTE.
           IF X ALPHABETIC-LOWER
              DISPLAY "Fail - Not alphabetic lower"
              END-DISPLAY
           END-IF.
           MOVE "a"     TO XBYTE.
           IF X NOT ALPHABETIC-LOWER
              DISPLAY "Fail - Alphabetic lower"
              END-DISPLAY
           END-IF.
           STOP RUN.

