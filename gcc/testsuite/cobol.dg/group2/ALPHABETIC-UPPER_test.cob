       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X            PIC X(04) VALUE "AAAA".
       01  FILLER REDEFINES X.
           03  XBYTE    PIC X.
           03  FILLER   PIC XXX.
       PROCEDURE        DIVISION.
           MOVE X"0D"   TO XBYTE.
           IF X ALPHABETIC-UPPER
              DISPLAY "Fail - Not alphabetic upper"
              END-DISPLAY
           END-IF.
           MOVE "A"     TO XBYTE.
           IF X NOT ALPHABETIC-UPPER
              DISPLAY "Fail - Alphabetic upper"
              END-DISPLAY
           END-IF.
           STOP RUN.

