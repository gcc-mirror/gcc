       *> { dg-do run }
       *> { dg-output-file "group2/259_PERFORM_VARYING_BY_-0.2.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
          77 X             PIC 9v9.
       PROCEDURE        DIVISION.
           PERFORM VARYING X FROM 0.8 BY -0.2
                   UNTIL   X < 0.4
             DISPLAY "X" NO ADVANCING
             END-DISPLAY
           END-PERFORM.
       IF X NOT = 0.2
         DISPLAY "WRONG X: " X END-DISPLAY
       END-IF
           STOP RUN.

