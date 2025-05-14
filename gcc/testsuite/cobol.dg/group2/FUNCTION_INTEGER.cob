       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X            PIC   S9(4)V9(4) VALUE -1.5.
       01  Y            PIC   9(12)      VALUE 600851475143.
       01  TEST-FLD     PIC S9(14)V9(08).
       PROCEDURE        DIVISION.
           MOVE FUNCTION INTEGER ( X )
             TO TEST-FLD.
           IF TEST-FLD NOT = -2
              DISPLAY 'INTEGER ( X ) wrong: ' TEST-FLD
              END-DISPLAY
           END-IF.
           MOVE FUNCTION INTEGER ( Y / 71 )
             TO TEST-FLD.
           IF TEST-FLD NOT = 8462696833
              DISPLAY 'INTEGER ( Y / 71 ) wrong: ' TEST-FLD
              END-DISPLAY
           END-IF.
           STOP RUN.

