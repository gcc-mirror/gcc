       *> { dg-do run }
       *> { dg-output-file "group2/PACKED-DECIMAL_numeric_test__2_.out" }

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
            DISPLAY "NG 1"
           ELSE
            DISPLAY "OK"
           END-IF.
           IF N-S2 IS NUMERIC
            DISPLAY "NG 2"
           ELSE
            DISPLAY "OK"
           END-IF.
           MOVE X"000c" TO X-2.
           IF N-2  IS NUMERIC
            DISPLAY "NG 3"
           ELSE
            DISPLAY "OK"
           END-IF.
           IF N-S2 IS NUMERIC
            DISPLAY "OK"
           ELSE
            DISPLAY "NG 4"
           END-IF.
           MOVE X"000d" TO X-2.
           IF N-2  IS NUMERIC
            DISPLAY "NG 5"
           ELSE
            DISPLAY "OK"
           END-IF.
           IF N-S2 IS NUMERIC
            DISPLAY "OK"
           ELSE
            DISPLAY "NG 6"
           END-IF.
           MOVE X"000f" TO X-2.
           IF N-2  IS NUMERIC
            DISPLAY "OK"
           ELSE
            DISPLAY "NG 7"
           END-IF.
           IF N-S2 IS NUMERIC
            DISPLAY "NG 8"
           ELSE
            DISPLAY "OK"
           END-IF.
           MOVE X"1234" TO X-2.
           IF N-2  IS NUMERIC
            DISPLAY "NG 9"
           ELSE
            DISPLAY "OK"
           END-IF.
           IF N-S2 IS NUMERIC
            DISPLAY "NG 10"
           ELSE
            DISPLAY "OK"
           END-IF.
           MOVE X"999f" TO X-2.
           IF N-2  IS NUMERIC
            DISPLAY "OK"
           ELSE
            DISPLAY "NG 11"
           END-IF.
           IF N-S2 IS NUMERIC
            DISPLAY "NG 12"
           ELSE
            DISPLAY "OK"
           END-IF.
           MOVE X"ffff" TO X-2.
           IF N-2  IS NUMERIC
            DISPLAY "NG 13"
           ELSE
            DISPLAY "OK"
           END-IF.
           IF N-S2 IS NUMERIC
            DISPLAY "NG 14"
           ELSE
            DISPLAY "OK"
           END-IF.
           STOP RUN.

