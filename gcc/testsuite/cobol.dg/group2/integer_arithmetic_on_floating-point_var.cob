       *> { dg-do run }

       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     prog.
       DATA            DIVISION.
       WORKING-STORAGE SECTION.
       01  x           USAGE COMP-1 VALUE 123.456.
       PROCEDURE       DIVISION.
           ADD 360 TO x
           IF x >  483.457 OR x <  483.455
               DISPLAY "ADD wrong: " x
               MOVE 483.456 TO x
           END-IF
           SUBTRACT 360 FROM x
           IF x >  123.457 OR x <  123.455
               DISPLAY "SUBTRACT wrong: " x
               MOVE 123.456 TO x
           END-IF
           DIVIDE 2 INTO x
           IF x >  61.729 OR x <  61.727
               DISPLAY "DIVIDE wrong: " x
               MOVE 61.728 TO x
           END-IF
           MULTIPLY 2 BY x
           IF x >  123.457 OR x <  123.455
               DISPLAY "MULTIPLY wrong: " x
           END-IF
           GOBACK.

