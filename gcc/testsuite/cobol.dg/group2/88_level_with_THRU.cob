       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  VAR-X        PIC X VALUE SPACE.
           88 X         VALUE "X".
           88 T-Y       VALUE "T" THRU "Y".
       01  VAR-9        PIC 9 VALUE ZERO.
           88 V9        VALUE 9.
           88 V2-4      VALUE 2 THRU 4.
       PROCEDURE        DIVISION.
           IF X
               DISPLAY "NOT OK '" VAR-X "' IS X"
               END-DISPLAY
           END-IF
           SET X TO TRUE
           IF NOT X
               DISPLAY "NOT OK '" VAR-X "' IS NOT X"
               END-DISPLAY
           END-IF
           IF NOT T-Y
               DISPLAY "NOT OK '" VAR-X "' IS NOT T-Y"
               END-DISPLAY
           END-IF
           SET T-Y TO TRUE
           IF NOT T-Y
               DISPLAY "NOT OK '" VAR-X "' IS NOT T-Y"
               END-DISPLAY
           END-IF
           MOVE 'Y' TO VAR-X
           IF NOT T-Y
               DISPLAY "NOT OK '" VAR-X "' IS NOT T-Y"
               END-DISPLAY
           END-IF
           MOVE 'Z' TO VAR-X
           IF T-Y
               DISPLAY "NOT OK '" VAR-X "' IS T-Y"
               END-DISPLAY
           END-IF
           MOVE 'A' TO VAR-X
           IF T-Y
               DISPLAY "NOT OK '" VAR-X "' IS T-Y"
               END-DISPLAY
           END-IF
           IF V9
               DISPLAY "NOT OK '" VAR-9 "' IS V9"
               END-DISPLAY
           END-IF
           SET V9 TO TRUE
           IF NOT V9
               DISPLAY "NOT OK '" VAR-9 "' IS NOT V9"
               END-DISPLAY
           END-IF
           SET V2-4 TO TRUE
           IF V9
               DISPLAY "NOT OK '" VAR-9 "' IS V9"
               END-DISPLAY
           END-IF
           IF NOT V2-4
               DISPLAY "NOT OK '" VAR-9 "' IS NOT V2-4"
               END-DISPLAY
           END-IF
           MOVE 3 TO VAR-9
           IF NOT V2-4
               DISPLAY "NOT OK '" VAR-9 "' IS NOT V2-4"
               END-DISPLAY
           END-IF
           MOVE 4 TO VAR-9
           IF NOT V2-4
               DISPLAY "NOT OK '" VAR-9 "' IS NOT V2-4"
               END-DISPLAY
           END-IF
           MOVE 5 TO VAR-9
           IF V2-4
               DISPLAY "NOT OK '" VAR-9 "' IS V2-4"
               END-DISPLAY
           END-IF
           MOVE 1 TO VAR-9
           IF V2-4
               DISPLAY "NOT OK '" VAR-9 "' IS V2-4"
               END-DISPLAY
           END-IF
           STOP RUN.

