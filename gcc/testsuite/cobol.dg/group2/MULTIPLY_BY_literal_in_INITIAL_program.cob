       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog INITIAL.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  num          PIC 9(4)    VALUE 5.
       01  result       PIC 9(4).
       01  ws-temp      PIC 9(8)V99.
       01  ws-temp2     PIC 9(3)V99 VALUE 10.50.
       PROCEDURE        DIVISION.
           MULTIPLY num BY 4 GIVING result
           MOVE 1.10          TO WS-TEMP.
           MULTIPLY WS-TEMP2  BY WS-TEMP GIVING WS-TEMP.

