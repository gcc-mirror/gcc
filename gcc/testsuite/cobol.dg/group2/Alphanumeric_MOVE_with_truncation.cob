       *> { dg-do run }
       *> { dg-options "-Wno-truncate" }
       *> { dg-output-file "group2/Alphanumeric_MOVE_with_truncation.out" }

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  x-left  PIC X(03).
       01  x-right PIC X(03) JUSTIFIED RIGHT.
       PROCEDURE DIVISION.
           MOVE '1234' TO x-left, x-right
           DISPLAY """" x-left """" space """" x-right """"
           IF x-left  not = '123'
           OR x-right not = '234'
              DISPLAY 'error with "1234":'
              END-DISPLAY
              DISPLAY x-left
              END-DISPLAY
              DISPLAY x-right
              END-DISPLAY
           END-IF
           MOVE '   3' TO x-left, x-right
           DISPLAY """" x-left """" space """" x-right """"
           IF x-left  not = spaces
           OR x-right not = '  3'
              DISPLAY 'error with "   3":'
              END-DISPLAY
              DISPLAY x-left
              END-DISPLAY
              DISPLAY x-right
              END-DISPLAY
           END-IF
           MOVE '3   ' TO x-left, x-right
           DISPLAY """" x-left """" space """" x-right """"
           IF x-left  not = '3'
           OR x-right not = spaces
              DISPLAY 'error with "3   ":'
              END-DISPLAY
              DISPLAY x-left
              END-DISPLAY
              DISPLAY x-right
              END-DISPLAY
           END-IF.

