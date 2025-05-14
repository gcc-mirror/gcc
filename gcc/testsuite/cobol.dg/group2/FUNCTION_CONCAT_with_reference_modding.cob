       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Y            PIC X(4).
       01  TEST-FLD     PIC X(9) VALUE SPACES.
       PROCEDURE        DIVISION.
           MOVE 'defx' TO Y.
           MOVE FUNCTION CONCAT
                ( Y "abc" "zz" "55" "666" ) (2 : 9)
             TO TEST-FLD.
           IF TEST-FLD NOT = 'efxabczz5'
              DISPLAY TEST-FLD
              END-DISPLAY
           END-IF.
           STOP RUN.

