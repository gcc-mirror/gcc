       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  TEST-FLD     PIC S9(04)V9(04).
       PROCEDURE        DIVISION.
           MOVE FUNCTION FRACTION-PART ( 3.12345 )
             TO TEST-FLD.
           IF TEST-FLD NOT = +0000.1234
              DISPLAY 'FRACTION-PART ( +3.12345 ) wrong: ' TEST-FLD
              END-DISPLAY
           END-IF.
           MOVE FUNCTION FRACTION-PART ( -3.12345 )
             TO TEST-FLD.
           IF TEST-FLD NOT = -0000.1234
              DISPLAY 'FRACTION-PART ( -3.12345 ) wrong: ' TEST-FLD
              END-DISPLAY
           END-IF.
           STOP RUN.

