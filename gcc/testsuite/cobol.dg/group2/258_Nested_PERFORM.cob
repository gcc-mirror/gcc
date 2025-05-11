       *> { dg-do run }
       *> { dg-output-file "group2/258_Nested_PERFORM.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       PROCEDURE        DIVISION.
           PERFORM 2 TIMES
             DISPLAY "X" NO ADVANCING
             END-DISPLAY
             PERFORM 2 TIMES
               DISPLAY "Y" NO ADVANCING
               END-DISPLAY
             END-PERFORM
           END-PERFORM.
           STOP RUN.

