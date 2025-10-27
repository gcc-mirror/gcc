       *> { dg-do run }
       *> { dg-output-file "group2/BLANK_WHEN_ZERO.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  x            PIC 9, BLANK WHEN ZERO, VALUE 1.
       PROCEDURE        DIVISION.
           DISPLAY "X should be 1: " """" x """"
           MOVE 0 TO x
           DISPLAY "X should be blank: " """" FUNCTION TRIM(x) """"
           MOVE ZERO TO x
           DISPLAY "X should be blank: " """" FUNCTION TRIM(x) """"
           GOBACK.

