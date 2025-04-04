       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  TEST-FLD     PIC S9(09)V9(02).
       PROCEDURE        DIVISION.
           MOVE FUNCTION INTEGER-OF-DATE ( 20000925 )
             TO TEST-FLD.
           IF TEST-FLD NOT = 000146000
              DISPLAY TEST-FLD
              END-DISPLAY
           END-IF.
           STOP RUN.

