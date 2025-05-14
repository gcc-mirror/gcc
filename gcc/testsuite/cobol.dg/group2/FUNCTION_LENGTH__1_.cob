       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X   PIC      S9(4)V9(4) VALUE -1.5.
       01  TEST-FLD     PIC S9(04)V9(02).
       PROCEDURE        DIVISION.
           MOVE FUNCTION LENGTH ( X )  TO TEST-FLD
           IF TEST-FLD NOT = 8
              DISPLAY 'LENGTH "00128" wrong: ' TEST-FLD
              END-DISPLAY
           END-IF

           MOVE FUNCTION LENGTH ( '00128' )
             TO TEST-FLD
           IF TEST-FLD NOT = 5
              DISPLAY 'LENGTH "00128" wrong: ' TEST-FLD
              END-DISPLAY
           END-IF

           MOVE FUNCTION LENGTH ( x'a0' )
             TO TEST-FLD
           IF TEST-FLD NOT = 1
              DISPLAY 'LENGTH x"a0" wrong: ' TEST-FLD
              END-DISPLAY
           END-IF

           MOVE FUNCTION LENGTH ( z'a0' )
             TO TEST-FLD
           IF TEST-FLD NOT = 3
              DISPLAY 'LENGTH z"a0" wrong: ' TEST-FLD
              END-DISPLAY
           END-IF

           STOP RUN.

