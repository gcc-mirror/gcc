       *> { dg-do run }
       *> { dg-output-file "group2/REDEFINES_values_on_FILLER_and_INITIALIZE.out" }

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA  DIVISION.
       WORKING-STORAGE SECTION.
       01  TSRDF.
           05  WS-ASK-ID-DATE                PIC X(10).
           05  WS-ASK-ID-DATE-R              REDEFINES WS-ASK-ID-DATE.
               10  WS-ASK-ID-DATE-YYYY       PIC 9(4) VALUE 2017.
               10  FILLER                    PIC X VALUE '-'.
               10  WS-ASK-ID-DATE-MM         PIC 9(2).
               10  FILLER                    PIC X VALUE '-'.
               10  WS-ASK-ID-DATE-DD         PIC 9(2).
       PROCEDURE DIVISION.
           MOVE ALL '*' TO WS-ASK-ID-DATE
           MOVE 2015 TO WS-ASK-ID-DATE-YYYY
           MOVE 08 TO WS-ASK-ID-DATE-MM
           MOVE 21 TO WS-ASK-ID-DATE-DD
           DISPLAY "The date is " WS-ASK-ID-DATE " Compiled".

           INITIALIZE WS-ASK-ID-DATE-R.
           MOVE 08 TO WS-ASK-ID-DATE-MM
           MOVE 21 TO WS-ASK-ID-DATE-DD
           DISPLAY "The date is " WS-ASK-ID-DATE " INITIALIZE".

           INITIALIZE WS-ASK-ID-DATE-R WITH FILLER.
           MOVE 08 TO WS-ASK-ID-DATE-MM
           MOVE 21 TO WS-ASK-ID-DATE-DD
           DISPLAY "The date is " WS-ASK-ID-DATE " WITH FILLER".

           INITIALIZE WS-ASK-ID-DATE-R WITH FILLER ALL TO VALUE.
           MOVE 08 TO WS-ASK-ID-DATE-MM
           MOVE 21 TO WS-ASK-ID-DATE-DD
           DISPLAY "The date is " WS-ASK-ID-DATE " ALL TO VALUE".
           STOP RUN.

