       *> { dg-do run }
       *> { dg-output-file "group2/MOVE_Z_literal_.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X            PIC XXXX.
       01  XRED REDEFINES X.
           03  XBYTE1   PIC X.
           03  XBYTE2   PIC X.
           03  XBYTE3   PIC X.
           03  XBYTE4   PIC X.
       PROCEDURE        DIVISION.
           MOVE Z"012" TO X.
           IF XBYTE1 = "0" AND
              XBYTE2 = "1" AND
              XBYTE3 = "2" AND
              XBYTE4 = LOW-VALUE
              DISPLAY "OK" NO ADVANCING
              END-DISPLAY
           ELSE
              DISPLAY "X = " X (1:3) NO ADVANCING
              END-DISPLAY
              IF XBYTE4 = LOW-VALUE
                 DISPLAY " WITH LOW-VALUE"
                 END-DISPLAY
              ELSE
                 DISPLAY " WITHOUT LOW-VALUE BUT '" XBYTE4 "'"
                 END-DISPLAY
              END-IF
           END-IF.
           STOP RUN.

