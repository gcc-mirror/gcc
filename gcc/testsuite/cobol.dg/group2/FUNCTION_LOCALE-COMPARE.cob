       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       PROCEDURE        DIVISION.
           IF FUNCTION LOCALE-COMPARE ("A", "B") NOT = "<"
              DISPLAY "Test 1 fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION LOCALE-COMPARE ("B", "A") NOT = ">"
              DISPLAY "Test 2 fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION LOCALE-COMPARE ("A", "A") NOT = "="
              DISPLAY "Test 3 fail"
              END-DISPLAY
           END-IF.
           STOP RUN.

