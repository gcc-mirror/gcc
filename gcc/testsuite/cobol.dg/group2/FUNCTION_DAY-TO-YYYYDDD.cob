       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  TEST-FLD     PIC S9(09)V9(02).
       PROCEDURE        DIVISION.
           MOVE FUNCTION DAY-TO-YYYYDDD ( 95005, -10, 2013 )
             TO TEST-FLD.
           IF TEST-FLD NOT = 001995005
              DISPLAY TEST-FLD
              END-DISPLAY
           END-IF.
           STOP RUN.

