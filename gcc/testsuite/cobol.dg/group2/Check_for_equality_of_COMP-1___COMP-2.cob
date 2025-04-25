       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  SRC1          COMP-2 VALUE 11.55.
       01  DST1          COMP-1.
       01  SRC2          COMP-1 VALUE 11.55.
       01  DST2          COMP-2.

       PROCEDURE        DIVISION.
           MOVE SRC1 TO DST1.
           IF DST1 not = 11.55
               DISPLAY 'error: move/compare FLOAT-LONG to FLOAT-SHORT failed ' DST1
               END-DISPLAY
           END-IF.

           MOVE SRC1 TO DST2.
           IF DST1 not = 11.55
               DISPLAY 'error: move/compare FLOAT-LONG to FLOAT-LONG failed ' DST2
               END-DISPLAY
           END-IF.

           MOVE ZERO TO DST1.
           MOVE ZERO TO DST2.

           MOVE SRC2 TO DST1.
           IF DST1 not = 11.55
               DISPLAY 'error: move/compare FLOAT-SHORT to FLOAT-SHORT failed: ' DST1
               END-DISPLAY
           END-IF.

           MOVE SRC2 TO DST2.
           IF DST2 not = 11.5500001907348633
               DISPLAY 'error: move/compare COMP-2 to literal failed: ' DST2
               END-DISPLAY
           END-IF.

           MOVE ZERO TO DST1.
           IF not (DST1 = 0 AND 0.0)
               DISPLAY "Zero compare failed: " DST1 END-DISPLAY
           END-IF.

           MOVE -0.0 TO DST1.
           IF not (DST1 = 0 AND 0.0)
               DISPLAY "Negative Zero compare failed: " DST1
               END-DISPLAY
           END-IF.

           MOVE 1.1234567 TO DST1.
           MOVE DST1 TO DST2.
           IF DST2 not = 1.12345671653747559
               DISPLAY "move/compare number to FLOAT to DOUBLE failed: "
                       DST1 " - " DST2
               END-DISPLAY
           END-IF.

      * Check for Tolerance
           MOVE 1.1234567 TO DST1.
           MOVE 1.1234568 TO DST2.
           IF DST1 = DST2 THEN
               DISPLAY 'move/compare of very near numbers failed (not identical): ' DST1 " - " DST2
               END-DISPLAY
           END-IF.

      * Within tolerance by definition, therefore not checked
      *     MULTIPLY 10000000000 BY DST1 DST2 END-MULTIPLY.
      *     IF DST1 = DST2 THEN
      *         DISPLAY "compare of very near numbers computed failed (id
      *-                "entical): " DST1 " - " DST2
      *         END-DISPLAY
      *     END-IF.

           MOVE 1.1234567 TO DST1.
           MOVE 1.1234569 TO DST2.
           IF DST1 = DST2 THEN
               DISPLAY 'move/compare of near equal numbers failed (identical): ' DST1 " - " DST2
               END-DISPLAY
           END-IF.

           MOVE 0.0001 TO DST1.
           MOVE 0.0000 TO DST2.
           IF DST1 = DST2 THEN
               DISPLAY 'move/compare of nearly equal very small numbers failed  (identical): ' DST1 " - " DST2
               END-DISPLAY
           END-IF.

           MOVE 1000001.0 TO DST1.
           MOVE 1000000.0 TO DST2.
           IF DST1 = DST2 THEN
               DISPLAY 'move/compare of nearly equal big numbers failed (identical): ' DST1 " - " DST2
               END-DISPLAY
           END-IF.

      * Within tolerance by definition, therefore not checked
      *     MOVE 1000000000.0 TO DST1.
      *     MOVE 1000000001.0 TO DST2.
      *     IF DST1 = DST2 THEN
      *         DISPLAY 'move/compare of nearly equal very big numbers fa
      *-                'iled (identical): ' DST1 " - " DST2
      *         END-DISPLAY
      *     END-IF.

           STOP RUN.

