       *> { dg-do run }
       *> { dg-output-file "group2/EXIT_PERFORM_CYCLE.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       PROCEDURE        DIVISION.
           PERFORM 2 TIMES
             DISPLAY "OK" NO ADVANCING
             END-DISPLAY
             EXIT PERFORM CYCLE
             DISPLAY "NOT OK"
             END-DISPLAY
           END-PERFORM
           STOP RUN.

