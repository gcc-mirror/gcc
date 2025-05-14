       *> { dg-do run }
       *> { dg-options "-dialect mf" }
       *> { dg-output-file "group2/COMP-6_numeric_test.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 G.
          02 X-2         PIC X(2).
          02 N-3         REDEFINES X-2 PIC 999  USAGE COMP-6.
          02 N-4         REDEFINES X-2 PIC 9999 USAGE COMP-6.
       PROCEDURE        DIVISION.
           MOVE X"0000" TO X-2.
           IF N-3  IS NUMERIC
            DISPLAY "OK"
            END-DISPLAY
           ELSE
            DISPLAY "1 NG"
            END-DISPLAY
           END-IF.
           IF N-4  IS NUMERIC
            DISPLAY "OK"
            END-DISPLAY
           ELSE
            DISPLAY "2 NG"
            END-DISPLAY
           END-IF.
           MOVE X"000c" TO X-2.
           IF N-3  IS NUMERIC
            DISPLAY "3 NG"
            END-DISPLAY
           ELSE
            DISPLAY "OK"
            END-DISPLAY
           END-IF.
           IF N-4  IS NUMERIC
            DISPLAY "4 NG"
            END-DISPLAY
           ELSE
            DISPLAY "OK"
            END-DISPLAY
           END-IF.
           MOVE X"1234" TO X-2.
           IF N-3  IS NUMERIC
            DISPLAY "5 NG"
            END-DISPLAY
           ELSE
            DISPLAY "OK"
            END-DISPLAY
           END-IF.
           IF N-4  IS NUMERIC
            DISPLAY "OK"
            END-DISPLAY
           ELSE
            DISPLAY "6 NG"
            END-DISPLAY
           END-IF.
           MOVE X"ffff" TO X-2.
           IF N-3  IS NUMERIC
            DISPLAY "7 NG"
            END-DISPLAY
           ELSE
            DISPLAY "OK"
            END-DISPLAY
           END-IF.
           IF N-4  IS NUMERIC
            DISPLAY "7 NG"
            END-DISPLAY
           ELSE
            DISPLAY "OK"
            END-DISPLAY
           END-IF.
           STOP RUN.

