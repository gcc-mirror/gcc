       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  TEST-FLD     PIC S9(09)V9(02).
       PROCEDURE        DIVISION.
           MOVE FUNCTION DATE-OF-INTEGER ( 146000 )
             TO TEST-FLD.
           IF TEST-FLD NOT = 20000925
              DISPLAY TEST-FLD
              END-DISPLAY
           END-IF.
           STOP RUN.

