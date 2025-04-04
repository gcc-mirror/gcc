       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Z            USAGE BINARY-LONG SIGNED.
       PROCEDURE        DIVISION.
           MOVE FUNCTION SIGN ( 3.12345 ) TO Z.
           IF Z NOT = 1
              DISPLAY "Sign 1 " Z
              END-DISPLAY
           END-IF.
           MOVE FUNCTION SIGN ( -0.0 ) TO Z.
           IF Z NOT = 0
              DISPLAY "Sign 2 " Z
              END-DISPLAY
           END-IF.
           MOVE FUNCTION SIGN ( 0.0 ) TO Z.
           IF Z NOT = 0
              DISPLAY "Sign 3 " Z
              END-DISPLAY
           END-IF.
           MOVE FUNCTION SIGN ( -3.12345 ) TO Z.
           IF Z NOT = -1
              DISPLAY "Sign 4 " Z
              END-DISPLAY
           END-IF.
           STOP RUN.

