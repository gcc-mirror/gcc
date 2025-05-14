       *> { dg-do run }
       *> { dg-output-file "group2/PACKED-DECIMAL_numeric_test__1_.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 G.
         02 X-2         PIC X(2).
         02 N-2         REDEFINES X-2 PIC 999  USAGE PACKED-DECIMAL.
         02 N-S2        REDEFINES X-2 PIC S999 USAGE PACKED-DECIMAL.
       PROCEDURE        DIVISION.
           MOVE X"0000" TO X-2.
           IF N-2  IS NUMERIC
            DISPLAY "1 NG"
            END-DISPLAY
           ELSE
            DISPLAY "OK"
            END-DISPLAY
           END-IF.
           IF N-S2 IS NUMERIC
            DISPLAY "2 NG"
            END-DISPLAY
           ELSE
            DISPLAY "OK"
            END-DISPLAY
           END-IF.
           MOVE X"000c" TO X-2.
           IF N-2  IS NUMERIC
            DISPLAY "3 NG"
            END-DISPLAY
           ELSE
            DISPLAY "OK"
            END-DISPLAY
           END-IF.
           IF N-S2 IS NUMERIC
            DISPLAY "OK"
            END-DISPLAY
           ELSE
            DISPLAY "4 NG"
            END-DISPLAY
           END-IF.
           MOVE X"000d" TO X-2.
           IF N-2  IS NUMERIC
            DISPLAY "5 NG"
            END-DISPLAY
           ELSE
            DISPLAY "OK"
            END-DISPLAY
           END-IF.
           IF N-S2 IS NUMERIC
            DISPLAY "OK"
            END-DISPLAY
           ELSE
            DISPLAY "6 NG"
            END-DISPLAY
           END-IF.
           MOVE X"000f" TO X-2.
           IF N-2  IS NUMERIC
            DISPLAY "OK"
            END-DISPLAY
           ELSE
            DISPLAY "7 NG"
            END-DISPLAY
           END-IF.
           IF N-S2 IS NUMERIC
            DISPLAY "8 NG"
            END-DISPLAY
           ELSE
            DISPLAY "OK"
            END-DISPLAY
           END-IF.
           MOVE X"1234" TO X-2.
           IF N-2  IS NUMERIC
            DISPLAY "9 NG"
            END-DISPLAY
           ELSE
            DISPLAY "OK"
            END-DISPLAY
           END-IF.
           IF N-S2 IS NUMERIC
            DISPLAY "10 NG"
            END-DISPLAY
           ELSE
            DISPLAY "OK"
            END-DISPLAY
           END-IF.
           MOVE X"999f" TO X-2.
           IF N-2  IS NUMERIC
            DISPLAY "OK"
            END-DISPLAY
           ELSE
            DISPLAY "11 NG"
            END-DISPLAY
           END-IF.
           IF N-S2 IS NUMERIC
            DISPLAY "12 NG"
            END-DISPLAY
           ELSE
            DISPLAY "OK"
            END-DISPLAY
           END-IF.
           MOVE X"ffff" TO X-2.
           IF N-2  IS NUMERIC
            DISPLAY "13 NG"
            END-DISPLAY
           ELSE
            DISPLAY "OK"
            END-DISPLAY
           END-IF.
           IF N-S2 IS NUMERIC
            DISPLAY "14 NG"
            END-DISPLAY
           ELSE
            DISPLAY "OK"
            END-DISPLAY
           END-IF.
           STOP RUN.

